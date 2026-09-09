module Sakuin.FetchCache where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent.STM
import GHC.Generics (Generic)
import Sakuin.Types

data Cached a
  = NotFetched
  | Missing
  | Found a
  deriving stock (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Cached a)

instance (FromJSON a) => FromJSON (Cached a)

data FetchCacheEntry = FetchCacheEntry
  { cachedNarInfo :: Cached NarInfo,
    cachedListing :: Cached FileNode
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FetchCacheEntry

instance FromJSON FetchCacheEntry

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

lookupCachedNarInfo :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Eff es (Cached NarInfo)
lookupCachedNarInfo cache storeHash =
  cachedNarInfo . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

lookupCachedListing :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Eff es (Cached FileNode)
lookupCachedListing cache storeHash =
  cachedListing . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

storeCachedNarInfo :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Maybe NarInfo -> Eff es ()
storeCachedNarInfo cache storeHash result =
  forM_ result $ \narinfo ->
    atomically . modifyTVar' (fetchCacheEntries cache) $
      Map.alter (Just . setNarInfo narinfo . maybe emptyFetchCacheEntry id) storeHash
  where
    setNarInfo narinfo entry = entry {cachedNarInfo = Found narinfo}

storeCachedListing :: forall es. (Concurrent :> es) => FetchCacheState -> StoreHash -> Maybe FileNode -> Eff es ()
storeCachedListing cache storeHash result =
  forM_ result $ \listing ->
    atomically . modifyTVar' (fetchCacheEntries cache) $
      Map.alter (Just . setListing listing . maybe emptyFetchCacheEntry id) storeHash
  where
    setListing listing entry = entry {cachedListing = Found listing}

encodeFetchCache :: Map StoreHash FetchCacheEntry -> LBS.ByteString
encodeFetchCache = Zstd.compress 3 . encode

decodeFetchCache :: LBS.ByteString -> Either String (Map StoreHash FetchCacheEntry)
decodeFetchCache = eitherDecode . Zstd.decompress
