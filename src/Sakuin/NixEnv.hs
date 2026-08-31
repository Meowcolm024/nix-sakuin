module Sakuin.NixEnv where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Map qualified as Map
import Sakuin.Types
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

data NixEnvPackage = NixEnvPackage
  { neSystem :: Text,
    neOutputName :: Text,
    neRawPath :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON NixEnvPackage where
  parseJSON = withObject "Package" $ \o -> do
    sys <- o .: "system"
    onm <- o .: "outputName"
    os <- o .:? "outputs" .!= mempty
    sp <- os .:? Key.fromText onm
    pure $ NixEnvPackage sys onm sp

toStoreEntry :: Attr -> NixEnvPackage -> Maybe StoreEntry
toStoreEntry attr pkg = do
  rawPath <- neRawPath pkg
  sp <- parseStorePath rawPath
  pure $ StoreEntry sp (Origin attr (neOutputName pkg) True (neSystem pkg))

normalizePackages :: Map Attr NixEnvPackage -> Packages
normalizePackages pkgs = Packages $ Map.foldlWithKey' insert Map.empty pkgs
  where
    insert acc attr pkg =
      case toStoreEntry attr pkg of
        Nothing -> acc
        Just se -> Map.insertWith preferShorter (spHash (storePath se)) se acc

queryPackages :: Text -> Maybe Text -> Maybe Text -> IO Packages
queryPackages nixpkgs system scope =
  readCreateProcessWithExitCode (proc "nix-env" args) "" >>= \case
    (ExitFailure _, _, err) ->
      fail $ "Failed to query packages: " <> err
    (ExitSuccess, out, _) ->
      case eitherDecode (fromString @LByteString out) of
        Left err -> fail $ "Failed to decode JSON: " <> err
        Right val -> pure $ normalizePackages val
  where
    args =
      [ "-qaP",
        "--out-path",
        "--json",
        "--arg",
        "config",
        "{ allowAliases = false; }",
        "--arg",
        "overlays",
        "[ ]",
        "--file",
        toString nixpkgs
      ]
        <> maybe [] (\sy -> ["--argstr", "system", toString sy]) system
        <> maybe [] (\sc -> ["-A", toString sc]) scope

queryAllScopes :: Text -> Maybe Text -> [Text] -> IO Packages
queryAllScopes nixpkgs system scopes =
  foldl' mergePackages (Packages Map.empty)
    <$> mapConcurrently (queryPackages nixpkgs system) (Nothing : map Just scopes)
  where
    mergePackages (Packages a) (Packages b) = Packages (Map.unionWith preferShorter a b)
