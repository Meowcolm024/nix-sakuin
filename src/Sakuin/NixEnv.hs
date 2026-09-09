module Sakuin.NixEnv where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Types
import System.Process.Typed

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

toStoreEntry :: Attr -> NixEnvPackage -> Maybe (WithOrigin StorePath)
toStoreEntry attr pkg = do
  rawPath <- neRawPath pkg
  sp <- parseStorePath rawPath
  pure $ WithOrigin (Origin attr (neOutputName pkg) True (neSystem pkg)) sp

normalizePackages :: Map Attr NixEnvPackage -> Packages
normalizePackages pkgs = Packages $ Map.foldlWithKey' insert Map.empty pkgs
  where
    insert acc attr pkg =
      case toStoreEntry attr pkg of
        Nothing -> acc
        Just se -> Map.insertWith preferShorter (spHash (value se)) se acc

parsePackages :: LBS.ByteString -> Either String Packages
parsePackages json = normalizePackages <$> eitherDecode json

data NixEnvError
  = NixEnvProcessError Text
  | NixEnvExitFailure Int Text
  | NixEnvDecodeError Text
  deriving stock (Show, Eq)

instance IsError NixEnvError where
  formatError = \case
    NixEnvProcessError message -> "failed to run nix-env: " <> message
    NixEnvExitFailure code message ->
      "nix-env failed with exit code " <> T.show code <> ": " <> message
    NixEnvDecodeError message -> "failed to decode nix-env output: " <> message

queryPackages ::
  forall es. (IOE :> es, Error NixEnvError :> es) => Text -> Maybe Text -> Maybe Text -> Eff es Packages
queryPackages nixpkgs system scope = do
  result <- try @SomeException $ readProcess (proc "nix-env" args)
  (ec, out, err) <- either (throwError . NixEnvProcessError . T.pack . displayException) pure result
  case ec of
    ExitFailure code ->
      throwError . NixEnvExitFailure code . decodeUtf8 $ LBS.toStrict err
    ExitSuccess ->
      case parsePackages out of
        Left de -> throwError . NixEnvDecodeError $ T.pack de
        Right val -> pure val
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
        T.unpack nixpkgs
      ]
        <> maybe [] (\sy -> ["--argstr", "system", T.unpack sy]) system
        <> maybe [] (\sc -> ["-A", T.unpack sc]) scope

queryAllScopes ::
  forall es.
  (IOE :> es, Concurrent :> es, Error NixEnvError :> es) =>
  Text -> Maybe Text -> [Maybe Text] -> Eff es Packages
queryAllScopes nixpkgs system scopes =
  foldl' mergePackages (Packages Map.empty)
    <$> mapConcurrently (queryPackages nixpkgs system) scopes
  where
    mergePackages (Packages a) (Packages b) = Packages (Map.unionWith preferShorter a b)
