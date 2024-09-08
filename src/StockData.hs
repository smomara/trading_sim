{-# LANGUAGE ForeignFunctionInterface #-}

module StockData where

import Types
import Foreign.C.String
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BS

foreign import ccall safe "fetch_stock_data"
    c_fetch_stock_data :: CString -> CString -> CString -> CString -> IO CString

foreign import ccall safe "free_string"
    c_free_string :: CString -> IO ()

fetchStockData :: String -> String -> String -> String -> IO (Either String [StockData])
fetchStockData symbol startDate endDate apiKey = do
    withCString symbol $ \cSymbol ->
        withCString startDate $ \cStartDate ->
            withCString endDate $ \cEndDate ->
                withCString apiKey $ \cApiKey -> do
                    cResult <- c_fetch_stock_data cSymbol cStartDate cEndDate cApiKey
                    result <- peekCString cResult
                    c_free_string cResult
                    return $ eitherDecode (BS.pack result)
