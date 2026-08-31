module Sakuin.Types where

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
  deriving stock (Show)

data Narinfo = Narinfo
  { niStorePath :: StorePath,
    niNarPath :: Text,
    niReferences :: [StoreEntry]
  }
  deriving stock (Show)
