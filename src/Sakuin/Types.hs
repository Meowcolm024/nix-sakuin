module Sakuin.Types where

import Data.ByteString.Char8 qualified as BS8
import Data.List (lookup)
import Data.Text qualified as T

type StoreHash = Text

type Attr = Text

data StorePath = StorePath
  { spDir :: Text,
    spHash :: StoreHash,
    spName :: Text
  }
  deriving stock (Show)

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
      let (suffix, prefix) = T.breakOnEnd (T.singleton c) t
       in if T.null prefix
            then ("", t)
            else (T.dropEnd 1 prefix, suffix)

data Package = Package
  { pAttr :: Attr,
    pSystem :: Text,
    pOutput :: Text,
    pPath :: StorePath
  }
  deriving stock (Show)

-- Keep shorter attr for entries with the same hash
preferShorter :: StoreEntry -> StoreEntry -> StoreEntry
preferShorter existing new
  | T.length (orAttr (origin new)) < T.length (orAttr (origin existing)) = new
  | otherwise = existing

data Origin = Origin
  { orAttr :: Attr,
    orOutput :: Text,
    orToplevel :: Bool,
    orSystem :: Text
  }
  deriving stock (Show)

data StoreEntry = StoreEntry
  { storePath :: StorePath,
    origin :: Origin
  }
  deriving stock (Show)

newtype Packages = Packages (Map StoreHash StoreEntry)
  deriving newtype (Show, Semigroup, Monoid)

data NarInfo = NarInfo
  { niStorePath :: StorePath,
    niNarPath :: Text,
    niReferences :: [StoreEntry]
  }
  deriving stock (Show)

parseNarInfoWith :: StoreEntry -> ByteString -> Maybe NarInfo
parseNarInfoWith (StoreEntry _ origin) bs = do
  let fields =
        [ (key, value)
        | line <- BS8.lines bs,
          let (key, rest) = BS8.break (== ':') line,
          not (BS8.null rest),
          let value = BS8.drop 1 rest
        ]
  niStorePath <- parseStorePath =<< lookupField fields "StorePath"
  niNarPath <- lookupField fields "URL"
  referenceTexts <- lookupFields fields "References"
  niRefPaths <- traverse parseStorePath referenceTexts
  let niReferences = map (\sp -> StoreEntry sp (origin {orToplevel = False})) niRefPaths
  pure $ NarInfo niStorePath niNarPath niReferences
  where
    lookupField fields key = T.strip . decodeUtf8 <$> lookup key fields
    lookupFields fields key = T.words <$> lookupField fields key

data FileNode
  = Regular {size :: Word64, executable :: Bool}
  | Symlink {target :: Text}
  | Directory {children :: Map Text FileNode}
  deriving stock (Show)

class (Monad m) => MonadCache m where
  fetchNarinfo :: StoreEntry -> m (Maybe NarInfo)
  fetchListing :: StoreEntry -> m (Maybe FileNode)
