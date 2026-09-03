import Sakuin.DatabaseSpec qualified as DatabaseSpec
import Sakuin.HydraSpec qualified as HydraSpec
import Sakuin.LogSpec qualified as LogSpec
import Sakuin.MockRegistrySpec qualified as MockRegistrySpec
import Sakuin.NixEnvSpec qualified as NixEnvSpec
import Sakuin.PipelineSpec qualified as PipelineSpec
import Sakuin.TypesSpec qualified as TypesSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "nix-sakuin"
      [ DatabaseSpec.tests,
        TypesSpec.tests,
        NixEnvSpec.tests,
        HydraSpec.tests,
        LogSpec.tests,
        MockRegistrySpec.tests,
        PipelineSpec.tests
      ]
