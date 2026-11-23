module Sakuin.Database.FrCode
  ( withFrCodeDatabase,
    runFrCodeSearch,
    runFrCodePureSearch,
    runFrCodeParallelSearch,
    runFrCodeNativeSearch,
    frCodeMatcher,
    foldFrCodeRows,
    searchFrCodeRows,
    searchFrCodeChunks,
  )
where

import Sakuin.Database.FrCode.Codec (foldFrCodeRows, withFrCodeDatabase)
import Sakuin.Database.FrCode.Native (runFrCodeNativeSearch)
import Sakuin.Database.FrCode.Parallel (runFrCodeParallelSearch, searchFrCodeChunks)
import Sakuin.Database.FrCode.Pure (runFrCodePureSearch)
import Sakuin.Database.FrCode.Query (frCodeMatcher)
import Sakuin.Database.FrCode.Serial (runFrCodeSearch, searchFrCodeRows)
