{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Data.List (find)
import Text.Printf (printf)
import qualified Data.Text as T
import Foreign.C.String
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)

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

data SimulationState = SimulationState
    { currentDay :: Int
    , portfolio :: TVar [(String, Int)]
    , cash :: TVar Double
    , stockData :: [StockData]
    }

data UIState = UIState
    { simState :: SimulationState
    , uiMode :: UIMode
    , buyQuantity :: String
    , sellQuantity :: String
    , portfolioValue :: Double
    , cashAmount :: Double
    , aapleShares :: Int
    }

data UIMode = NormalMode | BuyMode | SellMode
    deriving (Eq, Show)

data Name = StockList
    deriving (Ord, Eq, Show)

app :: App UIState e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr []
    }

drawUI :: UIState -> [Widget Name]
drawUI UIState{..} = [ui]
  where
    ui = borderWithLabel (str "Stock Market Simulator") $ 
            vBox [ drawStockInfo
                 , hBorder
                 , drawPortfolio
                 , hBorder
                 , drawControls
                 ]
    
    drawStockInfo = 
        let day = stockData simState !! currentDay simState
        in vBox
            [ str $ "Day " ++ show (currentDay simState + 1) ++ " - " ++ T.unpack (date day)
            , str $ formatStockData day
            ]
    
    drawPortfolio = vBox
        [ str $ "Portfolio Value: $" ++ printf "%.2f" portfolioValue
        , str $ "Cash: $" ++ printf "%.2f" cashAmount
        , str $ "AAPL Shares: " ++ show aapleShares
        ]
    
    drawControls = vBox
        [ str "Controls:"
        , str "b: Buy stocks"
        , str "s: Sell stocks"
        , str "n: Next day"
        , str "q: Quit simulation"
        , case uiMode of
            BuyMode -> str $ "Enter quantity to buy: " ++ buyQuantity
            SellMode -> str $ "Enter quantity to sell: " ++ sellQuantity
            _ -> emptyWidget
        ]

handleEvent :: BrickEvent Name e -> EventM Name UIState ()
handleEvent (VtyEvent (V.EvKey key [])) = do
    s <- get
    case (uiMode s, key) of
        (NormalMode, V.KChar 'b') -> put $ s { uiMode = BuyMode, buyQuantity = "" }
        (NormalMode, V.KChar 's') -> put $ s { uiMode = SellMode, sellQuantity = "" }
        (NormalMode, V.KChar 'n') -> do
            let newDay = currentDay (simState s) + 1
            if newDay < length (stockData (simState s))
                then do
                    newState <- liftIO $ updateUIState $ s { simState = (simState s) { currentDay = newDay } }
                    put newState
                else halt
        (NormalMode, V.KChar 'q') -> halt
        (BuyMode, V.KEnter) -> do
            let quantity = read (buyQuantity s) :: Int
                day = stockData (simState s) !! currentDay (simState s)
            result <- liftIO $ atomically $ do
                cashAmount <- readTVar (cash (simState s))
                if cashAmount < fromIntegral quantity * close day
                    then return $ Left "Insufficient funds"
                    else do
                        modifyTVar (cash (simState s)) (\c -> c - fromIntegral quantity * close day)
                        modifyTVar (portfolio (simState s)) (\p -> updatePortfolio p "AAPL" quantity)
                        return $ Right ()
            case result of
                Left err -> return ()  -- You might want to show an error message here
                Right _ -> do
                    newState <- liftIO $ updateUIState $ s { uiMode = NormalMode, buyQuantity = "" }
                    put newState
        (SellMode, V.KEnter) -> do
            let quantity = read (sellQuantity s) :: Int
                day = stockData (simState s) !! currentDay (simState s)
            result <- liftIO $ atomically $ do
                holdings <- readTVar (portfolio (simState s))
                case find (\(sym, _) -> sym == "AAPL") holdings of
                    Just (_, ownedQuantity) | ownedQuantity >= quantity -> do
                        modifyTVar (cash (simState s)) (\c -> c + fromIntegral quantity * close day)
                        modifyTVar (portfolio (simState s)) (\p -> updatePortfolio p "AAPL" (-quantity))
                        return $ Right ()
                    _ -> return $ Left "Insufficient stocks to sell"
            case result of
                Left err -> return ()  -- You might want to show an error message here
                Right _ -> do
                    newState <- liftIO $ updateUIState $ s { uiMode = NormalMode, sellQuantity = "" }
                    put newState
        (BuyMode, V.KChar c) -> put $ s { buyQuantity = buyQuantity s ++ [c] }
        (SellMode, V.KChar c) -> put $ s { sellQuantity = sellQuantity s ++ [c] }
        (BuyMode, V.KBS) -> put $ s { buyQuantity = take (length (buyQuantity s) - 1) (buyQuantity s) }
        (SellMode, V.KBS) -> put $ s { sellQuantity = take (length (sellQuantity s) - 1) (sellQuantity s) }
        (_, V.KEsc) -> put $ s { uiMode = NormalMode, buyQuantity = "", sellQuantity = "" }
        _ -> return ()
handleEvent _ = return ()

updateUIState :: UIState -> IO UIState
updateUIState s = do
    let day = stockData (simState s) !! currentDay (simState s)
    (portfolioValue, cashAmount, aapleShares) <- atomically $ do
        holdings <- readTVar (portfolio (simState s))
        cash <- readTVar (cash (simState s))
        let aapleShares = sum [qty | (sym, qty) <- holdings, sym == "AAPL"]
            stockValue = fromIntegral aapleShares * close day
        return (cash + stockValue, cash, aapleShares)
    return $ s { portfolioValue = portfolioValue, cashAmount = cashAmount, aapleShares = aapleShares }

updatePortfolio :: [(String, Int)] -> String -> Int -> [(String, Int)]
updatePortfolio [] symbol quantity = [(symbol, quantity)]
updatePortfolio ((sym, qty):rest) symbol quantity
    | sym == symbol = (sym, qty + quantity) : rest
    | otherwise = (sym, qty) : updatePortfolio rest symbol quantity

formatStockData :: StockData -> String
formatStockData StockData{..} = 
    printf "%s | Open: %.2f | High: %.2f | Low: %.2f | Close: %.2f | Volume: %.0f" 
        (T.unpack date) open high low close volume

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
            portfolioTVar <- newTVarIO []
            cashTVar <- newTVarIO 10000.0
            let simState = SimulationState
                    { currentDay = 0
                    , portfolio = portfolioTVar
                    , cash = cashTVar
                    , stockData = stockData
                    }
            initialState <- updateUIState UIState
                { simState = simState
                , uiMode = NormalMode
                , buyQuantity = ""
                , sellQuantity = ""
                , portfolioValue = 0
                , cashAmount = 0
                , aapleShares = 0
                }
            void $ defaultMain app initialState
