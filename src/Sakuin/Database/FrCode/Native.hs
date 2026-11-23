module Sakuin.Database.FrCode.Native (runFrCodeNativeSearch) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Foreign.C (CInt (..), CSize (..), CString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Sakuin.Search (SearchError (..))
import Sakuin.Types

foreign import ccall safe "sakuin_frcode_query"
  nativeQuery :: CString -> CString -> CString -> CString -> CString -> CInt -> CString -> CSize -> IO CInt

runFrCodeNativeSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeNativeSearch databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters -> do
    result <- liftIO $
      withText (T.pack databasePath) $ \database ->
        withText pattern $ \needle ->
          withOptionalText (filterPackage filters) $ \package ->
            withOptionalText (filterHash filters) $ \hash ->
              withText (T.pack $ filterTypes filters) $ \fileTypes ->
                allocaBytes 512 $ \message -> do
                  let flags = sum [bit | (enabled, bit) <- [(isRegex, 1), (filterAtRoot filters, 2), (filterWholeName filters, 4), (isMinimal, 8)], enabled]
                  status <- nativeQuery database needle package hash fileTypes flags message 512
                  if status == 0
                    then pure (Right ())
                    else do
                      detail <- decodeUtf8With lenientDecode <$> BS.packCString message
                      pure . Left $ if status == 2 then InvalidSearchRegex detail else SearchProcessError detail
    either throwError pure result
  where
    withText value = BS.useAsCString (encodeUtf8 value)
    withOptionalText = maybe ($ nullPtr) withText
