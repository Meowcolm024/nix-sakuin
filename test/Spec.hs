import Sakuin.HydraSpec qualified as HydraSpec
import Sakuin.NixEnvSpec qualified as NixEnvSpec
import Sakuin.TypesSpec qualified as TypesSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup "nix-sakuin" [TypesSpec.tests, NixEnvSpec.tests, HydraSpec.tests]
