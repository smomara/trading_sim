{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module StockData (fetchStockData) where

import Types
import Foreign.C.String
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

foreign import ccall safe "fetch_stock_data"
    c_fetch_stock_data :: CString -> CString -> CString -> CString -> IO CString

foreign import ccall safe "free_string"
    c_free_string :: CString -> IO ()

fetchStockData :: T.Text -> T.Text -> T.Text -> T.Text -> IO (Either String [StockData])
fetchStockData symbol startDate endDate apiKey = do
    withCString (T.unpack symbol) $ \cSymbol ->
        withCString (T.unpack startDate) $ \cStartDate ->
            withCString (T.unpack endDate) $ \cEndDate ->
                withCString (T.unpack apiKey) $ \cApiKey -> do
                    cResult <- c_fetch_stock_data cSymbol cStartDate cEndDate cApiKey
                    result <- peekCString cResult
                    c_free_string cResult
                    return $ eitherDecode (BS.fromStrict $ encodeUtf8 $ T.pack result)
