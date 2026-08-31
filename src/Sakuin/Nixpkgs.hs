module Sakuin.Nixpkgs where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

newtype Packages = Packages (Map Text Package)
  deriving stock (Show)

instance FromJSON Packages where
  parseJSON value = Packages <$> parseJSON value

data Package = Package
  { system :: Text,
    outputName :: Text,
    storePath :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \o -> do
    sys <- o .: "system"
    onm <- o .: "outputName"
    os <- o .:? "outputs" .!= mempty
    sp <- os .:? Key.fromText onm
    pure $ Package sys onm sp

queryPackages :: Text -> Maybe Text -> Maybe Text -> IO Packages
queryPackages nixpkgs system scope =
  readCreateProcessWithExitCode (proc "nix-env" args) "" >>= \case
    (ExitSuccess, out, _) -> do
      let decoded = eitherDecode (fromString @LByteString out)
      case decoded of
        Left err -> fail $ "Failed to decode JSON: " <> err
        Right val -> pure val
    (ExitFailure _, _, err) -> fail $ "Failed to query packages: " <> err
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
        <> maybe [] (\sys -> ["--argstr", "system", toString sys]) system
        <> maybe [] (\sc -> ["-A", "scope", toString sc]) scope
