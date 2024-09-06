{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.String
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf (printf)

data StockData = StockData
    { date :: Text
    , open :: Double
    , high :: Double
    , low :: Double
    , close :: Double
    , volume :: Double
    } deriving (Show)

instance FromJSON StockData where
    parseJSON = withObject "StockData" $ \v -> StockData
        <$> v .: "date"
        <*> v .: "open"
        <*> v .: "high"
        <*> v .: "low"
        <*> v .: "close"
        <*> v .: "volume"

foreign import ccall unsafe "fetch_stock_data"
    c_fetch_stock_data :: CString -> CString -> CString -> CString -> IO CString

foreign import ccall unsafe "free_string"
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
                    case eitherDecode (BS.pack result) of
                        Left err -> return $ Left $ "JSON parsing failed: " ++ err
                        Right stockData -> return $ Right stockData

formatStockData :: StockData -> String
formatStockData StockData{..} = 
    printf "%s | Open: %.2f | High: %.2f | Low: %.2f | Close: %.2f | Volume: %.0f" 
        (T.unpack date) open high low close volume

printSummary :: [StockData] -> IO ()
printSummary stockData = do
    let numDays = length stockData
        firstDay = head stockData
        lastDay = last stockData
        totalReturn = (close lastDay - close firstDay) / close firstDay * 100
    
    putStrLn $ "Summary for " ++ T.unpack (date firstDay) ++ " to " ++ T.unpack (date lastDay) ++ ":"
    putStrLn $ "Number of trading days: " ++ show numDays
    putStrLn $ "Starting price: $" ++ printf "%.2f" (close firstDay)
    putStrLn $ "Ending price: $" ++ printf "%.2f" (close lastDay)
    putStrLn $ "Total return: " ++ printf "%.2f%%" totalReturn
    putStrLn ""

main :: IO ()
main = do
    let symbol = "AAPL"
        startDate = "2023-01-01"
        endDate = "2023-12-31"
        apiKey = "LeHcyWk21YSgaBUxaz9PHevRNnThc7bF"
    result <- fetchStockData symbol startDate endDate apiKey
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right stockData -> do
            putStrLn $ "Fetched stock data for " ++ symbol ++ ":"
            printSummary stockData
            putStrLn "First 5 days:"
            mapM_ (putStrLn . formatStockData) (take 5 stockData)
            putStrLn "..."
            putStrLn "Last 5 days:"
            mapM_ (putStrLn . formatStockData) (reverse $ take 5 $ reverse stockData)
