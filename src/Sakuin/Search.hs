module Sakuin.Search where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Types
import System.Process.Typed
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt, matchTest)
import Text.Regex.TDFA.String (compile)

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

data SearchError
  = InvalidSearchRegex Text
  | SearchProcessError Text
  deriving stock (Show, Eq)

instance IsError SearchError where
  formatError = \case
    InvalidSearchRegex message -> "invalid search regex: " <> message
    SearchProcessError message -> message

runTsvSearch ::
  forall es a.
  (IOE :> es, Error SearchError :> es) =>
  FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runTsvSearch databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters ->
    searchTsvDatabase databasePath pattern isRegex filters $
      if isMinimal
        then \bs -> mapM_ (liftIO . LBS8.putStrLn) (Set.fromList $ LBS8.takeWhile (/= '\t') <$> bs)
        else mapM_ (liftIO . LBS8.putStrLn)

searchTsvDatabase ::
  forall es.
  (IOE :> es, Error SearchError :> es) =>
  FilePath -> Text -> Bool -> TsvSearchFilter -> ([LBS8.ByteString] -> Eff es ()) -> Eff es ()
searchTsvDatabase databasePath pattern isRegex filters sink =
  either (throwError . InvalidSearchRegex . T.pack) runSearch (pathMatcher pattern isRegex filters)
  where
    runSearch matchesPath = do
      result <- try @SomeException $
        withProcessWait zstdConfig $ \zstdProcess ->
          withProcessWait (setStdout createPipe . rgConfig $ getStdout zstdProcess) $ \rgProcess -> do
            output <- liftIO $ LBS8.hGetContents $ getStdout rgProcess
            sink $ filter (matchesTsvSearchFilter filters matchesPath) $ LBS8.lines output
            rgExit <- waitExitCode rgProcess
            case rgExit of
              ExitSuccess -> pure ()
              ExitFailure 1 -> pure ()
              ExitFailure code -> throwIO . userError $ "rg failed with exit code " <> show code
            checkExitCode zstdProcess
      either (throwError . SearchProcessError . T.pack . displayException) pure result
    zstdConfig =
      setStdout createPipe $ proc "zstd" ["--decompress", "--stdout", databasePath]
    rgConfig input =
      setStdin (useHandleOpen input) $
        proc "rg" (rgArguments pattern isRegex)

matchesTsvSearchFilter :: TsvSearchFilter -> PathMatcher -> LBS8.ByteString -> Bool
matchesTsvSearchFilter filters matchesPath line =
  case LBS8.split '\t' line of
    [package, metadata, fullPath] ->
      maybe True (`T.isPrefixOf` decode package) (filterPackage filters)
        && maybe True (hasStoreHash $ decode fullPath) (filterHash filters)
        && (null (filterTypes filters) || maybe False (`elem` filterTypes filters) (fileType metadata))
        && matchesPath (decode fullPath)
    _ -> False
  where
    decode = decodeUtf8 . LBS.toStrict
    fileType = fmap snd . LBS8.unsnoc

pathMatcher :: Text -> Bool -> TsvSearchFilter -> Either String PathMatcher
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

rgArguments :: Text -> Bool -> [String]
rgArguments pattern isRegex =
  ["--text", "--no-line-number", "--no-heading", "--color", "never"]
    <> (if isRegex then [] else ["--fixed-strings", "--ignore-case"])
    <> ["--", T.unpack pattern]
