module Sakuin.Types where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word64)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import GHC.Generics (Generic)

type StoreHash = Text

type Attr = Text

data StorePath = StorePath
  { spDir :: Text,
    spHash :: StoreHash,
    spName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StorePath

instance FromJSON StorePath

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
  deriving stock (Show, Eq, Generic)

instance ToJSON Origin

instance FromJSON Origin

data WithOrigin a = WithOrigin
  { origin :: Origin,
    value :: a
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (ToJSON a) => ToJSON (WithOrigin a)

instance (FromJSON a) => FromJSON (WithOrigin a)

newtype Packages = Packages (Map StoreHash (WithOrigin StorePath))
  deriving newtype (Show, Semigroup, Monoid)

data NarInfo = NarInfo
  { niStorePath :: StorePath,
    niNarPath :: Text,
    niReferences :: [StorePath]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NarInfo

instance FromJSON NarInfo

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
  | Directory {children :: a}
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

newtype FileNode = FileNode (FileNode' (Map Text FileNode))
  deriving newtype (Show, Eq)

instance FromJSON FileNode where
  parseJSON = withObject "FileNode" $ \o -> do
    nodeType <- o .: "type"
    case nodeType :: Text of
      "regular" -> FileNode <$> (Regular <$> o .: "size" <*> o .:? "executable" .!= False)
      "symlink" -> FileNode . Symlink <$> o .: "target"
      "directory" -> FileNode . Directory <$> o .: "entries"
      unknown -> fail $ "Unknown file node type: " <> T.unpack unknown

instance ToJSON FileNode where
  toJSON (FileNode node) = case node of
    Regular fileSize isExecutable ->
      object ["type" .= String "regular", "size" .= fileSize, "executable" .= isExecutable]
    Symlink linkTarget ->
      object ["type" .= String "symlink", "target" .= linkTarget]
    Directory entries ->
      object ["type" .= String "directory", "entries" .= entries]

newtype FileListing = FileListing {root :: FileNode}
  deriving newtype (Show, Eq)

instance FromJSON FileListing where
  parseJSON = withObject "FileListing" $ \o ->
    FileListing <$> o .: "root"

newtype FileLine = FileLine (Text, FileNode' ())
  deriving newtype (Show, Eq)

instance ToJSON FileLine where
  toJSON (FileLine (path, node)) = case node of
    Regular fileSize isExecutable ->
      object ["path" .= path, "type" .= String "regular", "size" .= fileSize, "executable" .= isExecutable]
    Symlink linkTarget ->
      object ["path" .= path, "type" .= String "symlink", "target" .= linkTarget]
    Directory () ->
      object ["path" .= path, "type" .= String "directory"]

type FileList = [FileLine]

toFileList :: FileNode -> FileList
toFileList = go ""
  where
    go path (FileNode (Regular fileSize isExecutable)) =
      [FileLine (path, Regular fileSize isExecutable)]
    go path (FileNode (Symlink linkTarget)) =
      [FileLine (path, Symlink linkTarget)]
    go path (FileNode (Directory entries)) =
      directoryEntry
        <> foldMap
          (\(name, node) -> go (appendPath path name) node)
          (Map.toAscList entries)
      where
        directoryEntry
          | path == "" = []
          | otherwise = [FileLine (path, Directory ())]
    appendPath "" name = "/" <> name
    appendPath path name = path <> "/" <> name

data IndexedStorePath = IndexedStorePath
  { indexedPath :: WithOrigin StorePath,
    indexedFiles :: FileNode
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON IndexedStorePath

instance FromJSON IndexedStorePath

data Fetch :: Effect where
  FetchNarInfo :: forall m. StorePath -> Fetch m (Maybe NarInfo)
  FetchListing :: forall m. StorePath -> Fetch m (Maybe FileNode)

type instance DispatchOf Fetch = Dynamic

fetchNarInfo :: forall es. (Fetch :> es) => StorePath -> Eff es (Maybe NarInfo)
fetchNarInfo = send . FetchNarInfo

fetchListing :: forall es. (Fetch :> es) => StorePath -> Eff es (Maybe FileNode)
fetchListing = send . FetchListing

data Database :: Effect where
  AddToDatabase :: forall m. IndexedStorePath -> Database m ()

type instance DispatchOf Database = Dynamic

addToDatabase :: forall es. (Database :> es) => IndexedStorePath -> Eff es ()
addToDatabase = send . AddToDatabase
