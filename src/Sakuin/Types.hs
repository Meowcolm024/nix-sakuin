module Sakuin.Types where

import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans qualified as Trans
import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Text qualified as T

type StoreHash = Text

type Attr = Text

data StorePath = StorePath
  { spDir :: Text,
    spHash :: StoreHash,
    spName :: Text
  }
  deriving stock (Show, Eq)

parseStorePath :: Text -> Maybe StorePath
parseStorePath path = do
  (prefix, name) <- splitOnce '-' path
  let (hash, storeDir) = splitLast '/' prefix
  pure $ StorePath storeDir hash name
  where
    splitOnce c t =
      let (a, b) = T.break (== c) t
       in if T.null b
            then Nothing
            else Just (a, T.drop 1 b)
    splitLast c t =
      let (prefix, suffix) = T.breakOnEnd (T.singleton c) t
       in if T.null prefix
            then ("", t)
            else (suffix, T.dropEnd 1 prefix)

data Package = Package
  { pAttr :: Attr,
    pSystem :: Text,
    pOutput :: Text,
    pPath :: StorePath
  }
  deriving stock (Show)

-- Keep shorter attr for entries with the same hash
preferShorter :: WithOrigin a -> WithOrigin a -> WithOrigin a
preferShorter existing candidate
  | T.length (orAttr (origin candidate)) < T.length (orAttr (origin existing)) = candidate
  | otherwise = existing

data Origin = Origin
  { orAttr :: Attr,
    orOutput :: Text,
    orToplevel :: Bool,
    orSystem :: Text
  }
  deriving stock (Show, Eq)

data WithOrigin a = WithOrigin
  { origin :: Origin,
    value :: a
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

newtype Packages = Packages (Map StoreHash (WithOrigin StorePath))
  deriving newtype (Show, Semigroup, Monoid)

data NarInfo = NarInfo
  { niStorePath :: StorePath,
    niNarPath :: Text,
    niReferences :: [StorePath]
  }
  deriving stock (Show, Eq)

parseNarInfo :: ByteString -> Maybe NarInfo
parseNarInfo bs = do
  let fields =
        [ (key, fieldValue)
        | line <- BS8.lines bs,
          let (key, rest) = BS8.break (== ':') line,
          not (BS8.null rest),
          let fieldValue = BS8.drop 1 rest
        ]
  niStorePath <- parseStorePath =<< lookupField fields "StorePath"
  niNarPath <- lookupField fields "URL"
  referenceTexts <- lookupFields fields "References"
  niReferences <- traverse (parseReference (spDir niStorePath)) referenceTexts
  pure $ NarInfo niStorePath niNarPath niReferences
  where
    lookupField fields key = T.strip . decodeUtf8 <$> lookup key fields
    lookupFields fields key = T.words <$> lookupField fields key
    parseReference storeDir reference
      | "/" `T.isPrefixOf` reference = parseStorePath reference
      | otherwise = parseStorePath (storeDir <> "/" <> reference)

data FileNode' a
  = Regular {size :: Word64, executable :: Bool}
  | Symlink {target :: Text}
  | Directory {children :: Map Text a}
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

newtype FileNode = FileNode (FileNode' (FileNode))
  deriving newtype (Show, Eq)

instance FromJSON FileNode where
  parseJSON = withObject "FileNode" $ \o -> do
    nodeType <- o .: "type"
    case nodeType :: Text of
      "regular" -> FileNode <$> (Regular <$> o .: "size" <*> o .:? "executable" .!= False)
      "symlink" -> FileNode . Symlink <$> o .: "target"
      "directory" -> FileNode . Directory <$> o .: "entries"
      unknown -> fail $ "Unknown file node type: " <> toString unknown

newtype FileListing = FileListing {root :: FileNode}
  deriving newtype (Show, Eq)

instance FromJSON FileListing where
  parseJSON = withObject "FileListing" $ \o ->
    FileListing <$> o .: "root"

type FileList = [(Text, FileNode' Void)]

toFileList :: FileNode -> FileList
toFileList = go ""
  where
    go path (FileNode (Regular fileSize isExecutable)) =
      [(path, Regular fileSize isExecutable)]
    go path (FileNode (Symlink linkTarget)) =
      [(path, Symlink linkTarget)]
    go path (FileNode (Directory entries)) =
      foldMap
        (\(name, node) -> go (appendPath path name) node)
        (Map.toAscList entries)
    appendPath "" name = "/" <> name
    appendPath path name = path <> "/" <> name

data IndexedStorePath = IndexedStorePath
  { indexedPath :: WithOrigin StorePath,
    indexedFiles :: FileNode
  }
  deriving stock (Show, Eq)

class (Monad m) => MonadCache m where
  fetchNarinfo :: StorePath -> m (Maybe NarInfo)
  fetchListing :: StorePath -> m (Maybe FileNode)

class (Monad m) => MonadDatabase m where
  addToDatabase :: IndexedStorePath -> m ()

newtype DatabaseT d m a = DatabaseT
  { unDatabaseT :: ReaderT d m a
  }
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

instance MonadTrans (DatabaseT d) where
  lift = DatabaseT . Trans.lift

runDatabaseT :: d -> DatabaseT d m a -> m a
runDatabaseT database = (`runReaderT` database) . unDatabaseT
