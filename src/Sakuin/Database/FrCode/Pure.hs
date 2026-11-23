module Sakuin.Database.FrCode.Pure (runFrCodePureSearch) where

import Control.Monad (void)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static (Error)
import Sakuin.Database.FrCode.Codec (foldFrCodePackages)
import Sakuin.Database.FrCode.Query (frCodeMatcherWith, printFrCodeMatches, runFrCodeSearchWith)
import Sakuin.Search (SearchError)
import Sakuin.Types (Search)

runFrCodePureSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodePureSearch = runFrCodeSearchWith (frCodeMatcherWith False) $ \isMinimal matchesPath output ->
  void . foldFrCodePackages (printFrCodeMatches isMinimal matchesPath output) Set.empty
