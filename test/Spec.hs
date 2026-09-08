import Sakuin.DatabaseSpec qualified as DatabaseSpec
import Sakuin.FetchCacheSpec qualified as FetchCacheSpec
import Sakuin.HydraSpec qualified as HydraSpec
import Sakuin.LogSpec qualified as LogSpec
import Sakuin.MockRegistrySpec qualified as MockRegistrySpec
import Sakuin.NixEnvSpec qualified as NixEnvSpec
import Sakuin.PipelineSpec qualified as PipelineSpec
import Sakuin.ProgressSpec qualified as ProgressSpec
import Sakuin.SearchSpec qualified as SearchSpec
import Sakuin.TypesSpec qualified as TypesSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "nix-sakuin"
      [ DatabaseSpec.tests,
        FetchCacheSpec.tests,
        TypesSpec.tests,
        SearchSpec.tests,
        NixEnvSpec.tests,
        HydraSpec.tests,
        LogSpec.tests,
        MockRegistrySpec.tests,
        PipelineSpec.tests,
        ProgressSpec.tests
      ]
