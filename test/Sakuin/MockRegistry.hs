module Sakuin.MockRegistry where

import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Sakuin
import Sakuin.Database

data MockRegistryConfig = MockRegistryConfig
  { mockEntryCount :: Int,
    mockSeedCount :: Int,
    mockExternalReferenceCount :: Int
  }
  deriving stock (Show, Eq)

defaultMockRegistryConfig :: MockRegistryConfig
defaultMockRegistryConfig =
  MockRegistryConfig
    { mockEntryCount = 100,
      mockSeedCount = 10,
      mockExternalReferenceCount = 5
    }

-- | The origin-free truth stored by the mock registry.
data MockRegistryEntry = MockRegistryEntry
  { mockPath :: StorePath,
    mockFiles :: FileNode,
    mockReferences :: [StorePath]
  }
  deriving stock (Show, Eq)

data MockRegistry = MockRegistry
  { mrEntries :: Map StoreHash MockRegistryEntry,
    mrSeeds :: Packages,
    mrExternalPaths :: [StorePath],
    mrExpected :: Map StoreHash IndexedStorePath
  }
  deriving stock (Show)

newtype MockCache a = MockCache {unMockCache :: Reader MockRegistry a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadCache MockCache where
  fetchNarinfo path = MockCache $ asks $ \registry -> do
    entry <- Map.lookup (spHash path) (mrEntries registry)
    pure
      NarInfo
        { niStorePath = mockPath entry,
          niNarPath = "nar/" <> spHash (mockPath entry) <> ".nar.xz",
          niReferences = mockReferences entry
        }

  fetchListing path =
    MockCache $ asks (fmap mockFiles . Map.lookup (spHash path) . mrEntries)

runMockCache :: MockRegistry -> MockCache a -> a
runMockCache registry = (`runReader` registry) . unMockCache

data MockPipelineEnvironment = MockPipelineEnvironment
  { mpeRegistry :: MockRegistry,
    mpeDatabase :: MemoryDatabase
  }

newtype MockPipeline a = MockPipeline {unMockPipeline :: ReaderT MockPipelineEnvironment IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask
    )

instance MonadCache MockPipeline where
  fetchNarinfo path = MockPipeline $ asks $ \environment -> do
    entry <- Map.lookup (spHash path) (mrEntries (mpeRegistry environment))
    pure
      NarInfo
        { niStorePath = mockPath entry,
          niNarPath = "nar/" <> spHash (mockPath entry) <> ".nar.xz",
          niReferences = mockReferences entry
        }

  fetchListing path =
    MockPipeline $ asks (fmap mockFiles . Map.lookup (spHash path) . mrEntries . mpeRegistry)

instance MonadDatabase MockPipeline where
  addToDatabase indexed = MockPipeline $ do
    database <- asks mpeDatabase
    liftIO $ insertMemoryDatabase database indexed

-- | Exercise the real concurrent pipeline against the generated cache and an
-- in-memory database, returning everything the pipeline emitted.
runMockPipeline :: Int -> MockRegistry -> IO (Map StoreHash IndexedStorePath)
runMockPipeline workerCount registry = do
  database <- newMemoryDatabase
  let environment = MockPipelineEnvironment registry database
  runReaderT (unMockPipeline (runPipeline workerCount (mrSeeds registry))) environment
  readMemoryDatabase database

mockEntries :: MockRegistry -> Map StoreHash MockRegistryEntry
mockEntries = mrEntries

mockSeeds :: MockRegistry -> Packages
mockSeeds = mrSeeds

mockExternalPaths :: MockRegistry -> [StorePath]
mockExternalPaths = mrExternalPaths

-- | The exact reachable result. Seed paths are top-level; all paths reached
-- through references inherit the seed origin with @orToplevel = False@.
mockExpected :: MockRegistry -> Map StoreHash IndexedStorePath
mockExpected = mrExpected

-- | Generate a deterministic registry. Internal references form several
-- components and include shared dependencies and cycles. Some entries also
-- reference paths absent from the registry, for which 'MonadCache' returns
-- 'Nothing'.
generateMockRegistry :: MockRegistryConfig -> MockRegistry
generateMockRegistry config = registry
  where
    entryCount = max 0 (mockEntryCount config)
    seedCount = min entryCount (max 0 (mockSeedCount config))
    externalCount = max 0 (mockExternalReferenceCount config)

    paths = map (storePath "mock-package") [0 .. entryCount - 1]
    externals = map (storePath "external-package") [entryCount .. entryCount + externalCount - 1]
    entries =
      Map.fromList
        [ (spHash path, makeEntry index path paths externals)
        | (index, path) <- zip [0 ..] paths
        ]
    seedEntries =
      Map.fromList
        [ (spHash path, WithOrigin (seedOrigin index) path)
        | (index, path) <- take seedCount (zip [0 ..] paths)
        ]
    seeds = Packages seedEntries
    registry =
      MockRegistry
        { mrEntries = entries,
          mrSeeds = seeds,
          mrExternalPaths = externals,
          mrExpected = expectedEntries entries seedEntries
        }

makeEntry :: Int -> StorePath -> [StorePath] -> [StorePath] -> MockRegistryEntry
makeEntry index path paths externals =
  MockRegistryEntry
    { mockPath = path,
      mockFiles = mockListing index,
      mockReferences = internalReferences <> externalReferences
    }
  where
    pathCount = length paths
    at candidate
      | candidate >= 0 && candidate < pathCount = maybeToList (listToMaybe (drop candidate paths))
      | otherwise = []
    -- Boundaries at 3, 7, ... leave disconnected components. The second edge
    -- creates shared descendants, while the backward edge introduces cycles.
    internalReferences =
      at (index + 1)
        <> (if index `mod` 4 == 0 then at (index + 2) else [])
        <> (if index `mod` 7 == 6 then at (index - 1) else [])
          & filter (\reference -> index `mod` 4 /= 3 || spHash reference < spHash path)
    externalReferences =
      case externals of
        [] -> []
        _
          | index `mod` 5 == 0 -> maybeToList (listToMaybe (drop (index `mod` length externals) externals))
          | otherwise -> []

expectedEntries ::
  Map StoreHash MockRegistryEntry ->
  Map StoreHash (WithOrigin StorePath) ->
  Map StoreHash IndexedStorePath
expectedEntries entries seeds = Map.mapMaybeWithKey toIndexed origins
  where
    origins =
      Map.foldlWithKey'
        (\acc seedHash seed -> propagate entries Set.empty seedHash (origin seed) (value seed) acc)
        Map.empty
        seeds
    toIndexed hash pathWithOrigin = do
      entry <- Map.lookup hash entries
      pure $ IndexedStorePath pathWithOrigin (mockFiles entry)

propagate ::
  Map StoreHash MockRegistryEntry ->
  Set StoreHash ->
  StoreHash ->
  Origin ->
  StorePath ->
  Map StoreHash (WithOrigin StorePath) ->
  Map StoreHash (WithOrigin StorePath)
propagate entries visited seedHash seed path accumulated
  | Set.member hash visited = accumulated
  | otherwise =
      case Map.lookup hash entries of
        Nothing -> accumulated
        Just entry ->
          foldl'
            (\result reference -> propagate entries visited' seedHash seed reference result)
            accumulated'
            (mockReferences entry)
  where
    hash = spHash path
    visited' = Set.insert hash visited
    candidate = WithOrigin (originFor path) path
    accumulated' = Map.insertWith preferOrigin hash candidate accumulated
    originFor current
      | spHash current == seedHash = seed
      | otherwise = seed {orToplevel = False}

preferOrigin :: WithOrigin a -> WithOrigin a -> WithOrigin a
preferOrigin existing candidate
  | orToplevel (origin candidate) && not (orToplevel (origin existing)) = candidate
  | orToplevel (origin existing) && not (orToplevel (origin candidate)) = existing
  | otherwise = preferShorter existing candidate

seedOrigin :: Int -> Origin
seedOrigin index =
  Origin
    { orAttr = "mock.package." <> show index,
      orOutput = "out",
      orToplevel = True,
      orSystem = "x86_64-linux"
    }

storePath :: Text -> Int -> StorePath
storePath name index =
  StorePath
    { spDir = "/nix/store",
      spHash = T.justifyRight 32 '0' (show (index + 1)),
      spName = name <> "-" <> show index <> "-1.0"
    }

mockListing :: Int -> FileNode
mockListing index =
  FileNode . Directory $
    Map.fromList
      [ ( "bin",
          FileNode . Directory $
            Map.singleton
              ("mock-tool-" <> show index)
              (FileNode $ Regular (fromIntegral (4096 + index)) True)
        ),
        ( "share",
          FileNode . Directory $ Map.singleton "current" (FileNode $ Symlink "../bin")
        )
      ]
