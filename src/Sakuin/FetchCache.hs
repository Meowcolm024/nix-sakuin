module Sakuin.FetchCache where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.STM
import GHC.Generics (Generic)
import Sakuin.Types

data Cached
  = NotFetched
  | Missing
  | Found ByteString
  deriving stock (Show, Eq, Generic)

instance Serialise Cached

data FetchCacheEntry = FetchCacheEntry
  { cachedNarInfo :: Cached,
    cachedListing :: Cached
  }
  deriving stock (Show, Eq, Generic)

instance Serialise FetchCacheEntry

data FetchCacheFile = FetchCacheFile
  { cacheFormatVersion :: Word,
    cacheEntries :: Map StoreHash FetchCacheEntry
  }
  deriving stock (Show, Eq, Generic)

instance Serialise FetchCacheFile

fetchCacheFormatVersion :: Word
fetchCacheFormatVersion = 1

newtype FetchCacheState = FetchCacheState
  { fetchCacheEntries :: TVar (Map StoreHash FetchCacheEntry)
  }

emptyFetchCacheEntry :: FetchCacheEntry
emptyFetchCacheEntry = FetchCacheEntry NotFetched NotFetched

newFetchCacheState :: forall es. (Concurrent :> es) => Map StoreHash FetchCacheEntry -> Eff es FetchCacheState
newFetchCacheState entries = FetchCacheState <$> newTVarIO entries

-- note that it loads the whole cache to memory...
readFetchCacheState :: forall es. (Concurrent :> es) => FetchCacheState -> Eff es (Map StoreHash FetchCacheEntry)
readFetchCacheState = readTVarIO . fetchCacheEntries

lookupCachedNarInfo :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Eff es Cached
lookupCachedNarInfo cache storeHash =
  cachedNarInfo . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

lookupCachedListing :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Eff es Cached
lookupCachedListing cache storeHash =
  cachedListing . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

storeCachedNarInfo :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Cached -> Eff es ()
storeCachedNarInfo cache storeHash result =
  atomically . modifyTVar' (fetchCacheEntries cache) $
    Map.alter (Just . setNarInfo . maybe emptyFetchCacheEntry id) storeHash
  where
    setNarInfo entry = entry {cachedNarInfo = result}

storeCachedListing :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Cached -> Eff es ()
storeCachedListing cache storeHash result =
  atomically . modifyTVar' (fetchCacheEntries cache) $
    Map.alter (Just . setListing . maybe emptyFetchCacheEntry id) storeHash
  where
    setListing entry = entry {cachedListing = result}

encodeFetchCache :: Map StoreHash FetchCacheEntry -> LBS.ByteString
encodeFetchCache = serialise . FetchCacheFile fetchCacheFormatVersion

decodeFetchCache :: LBS.ByteString -> Either Text (Map StoreHash FetchCacheEntry)
decodeFetchCache bytes = do
  FetchCacheFile version entries <- either (Left . T.show) Right $ deserialiseOrFail bytes
  if version == fetchCacheFormatVersion
    then Right entries
    else Left $ "unsupported fetch cache version " <> T.show version
