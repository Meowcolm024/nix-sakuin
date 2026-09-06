module Sakuin.Search where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Sakuin.Types
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt, matchTest)
import Text.Regex.TDFA.String (compile)

type PathMatcher = Text -> Bool

type SearchResult = (WithOrigin StorePath, FileLine)

data Search :: Effect where
  SearchPaths :: forall m. PathMatcher -> Search m ()

type instance DispatchOf Search = Dynamic

searchPaths :: forall es. (Search :> es) => PathMatcher -> Eff es ()
searchPaths = send . SearchPaths

keywordMatcher :: Text -> PathMatcher
keywordMatcher query candidate =
  all (`T.isInfixOf` T.toCaseFold candidate) (T.words $ T.toCaseFold query)

suffixMatcher :: Text -> PathMatcher
suffixMatcher = T.isSuffixOf

keywordSuffixMatcher :: Text -> PathMatcher
keywordSuffixMatcher query
  | "/" `T.isInfixOf` query = suffixMatcher query
  | otherwise = keywordMatcher query

regexMatcher :: Text -> Either String PathMatcher
regexMatcher pattern = do
  regex <- compile defaultCompOpt defaultExecOpt (T.unpack pattern)
  pure $ matchTest regex . T.unpack
