module Sakuin.Search where

import Data.Text (Text)
import Data.Text qualified as T
import Sakuin.Types
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt, matchTest)
import Text.Regex.TDFA.String (compile)

data SearchError = InvalidSearchRegex Text | SearchProcessError Text
  deriving stock (Show, Eq)

instance IsError SearchError where
  formatError = \case
    InvalidSearchRegex message -> "invalid search regex: " <> message
    SearchProcessError message -> message

keywordMatcher :: Text -> PathMatcher
keywordMatcher query candidate =
  all (`T.isInfixOf` T.toCaseFold candidate) (T.words $ T.toCaseFold query)

suffixMatcher :: Text -> PathMatcher
suffixMatcher = T.isSuffixOf

regexMatcher :: Text -> Either String PathMatcher
regexMatcher pattern = do
  regex <- compile defaultCompOpt defaultExecOpt (T.unpack pattern)
  pure $ matchTest regex . T.unpack

-- Heuristic: the first slash-separated component beginning with a canonical
-- 32-character hash and a dash is the store-path basename. Everything after
-- it is treated as the path within that store object.
listingParts :: Text -> Maybe (StorePath, Text)
listingParts fullPath = do
  let (storeDir, rest) = break isStoreBaseName $ T.splitOn "/" fullPath
  (storeBaseName, listing) <- case rest of
    [] -> Nothing
    first : remaining -> Just (first, remaining)
  if null listing
    then Nothing
    else do
      storePath <- parseStorePath $ T.intercalate "/" (storeDir <> [storeBaseName])
      pure (storePath, "/" <> T.intercalate "/" listing)
  where
    isStoreBaseName component =
      let (hash, name) = T.breakOn "-" component
       in T.length hash == 32 && not (T.null name)

isListingOf :: Text -> Text -> Bool
isListingOf fullPath atRootPath =
  maybe False ((== ensureLeadingSlash atRootPath) . snd) $ listingParts fullPath

hasStoreHash :: Text -> StoreHash -> Bool
hasStoreHash fullPath expectedHash =
  maybe False ((== expectedHash) . spHash . fst) $ listingParts fullPath

listingPathMatches :: Text -> PathMatcher -> Bool
listingPathMatches fullPath matches =
  maybe False (matches . snd) $ listingParts fullPath

ensureLeadingSlash :: Text -> Text
ensureLeadingSlash value
  | "/" `T.isPrefixOf` value = value
  | otherwise = "/" <> value

pathMatcher :: Text -> Bool -> SearchFilter -> Either String PathMatcher
pathMatcher pattern isRegex filters
  | isRegex = (\matches fullPath -> listingPathMatches fullPath matches) <$> regexMatcher anchoredPattern
  | otherwise = Right $ fixedMatcher (T.toCaseFold pattern)
  where
    anchoredPattern
      | filterAtRoot filters && filterWholeName filters = wrap "^(" ")$"
      | filterAtRoot filters = wrap "^(" ")"
      | filterWholeName filters = wrap "(" ")$"
      | otherwise = pattern
    wrap before after = before <> ensureLeadingSlash pattern <> after
    fixedMatcher query fullPath
      | filterAtRoot filters && filterWholeName filters =
          isListingOf (T.toCaseFold fullPath) query
      | filterAtRoot filters =
          listingPathMatches fullPath (ensureLeadingSlash query `T.isPrefixOf`)
      | otherwise = listingPathMatches fullPath (matchesFixed query)
    matchesFixed query candidate
      | filterWholeName filters = query `T.isSuffixOf` path
      | otherwise = query `T.isInfixOf` path
      where
        path = T.toCaseFold candidate
