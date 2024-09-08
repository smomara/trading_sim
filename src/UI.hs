{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module UI 
    ( UIState(..)
    , UIMode(..)
    , Name(..)
    , drawUI
    , handleEvent
    , runUI
    ) where

import Types
import qualified Simulation
import qualified StockData
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Concurrent.STM (newTVarIO)
import Text.Printf (printf)
import qualified Data.Text as T

drawUI :: UIState -> [Widget Name]
drawUI UIState{..} = [ui]
  where
    ui = case uiMode of
        ApiKeyEntry -> drawApiKeyEntry apiKey
        StockSymbolEntry -> drawStockSymbolEntry stockSymbol
        _ -> drawMainUI UIState{..}

drawApiKeyEntry :: String -> Widget Name
drawApiKeyEntry apiKey = 
    centerLayer $ 
    borderWithLabel (str "Enter Polygon API Key") $ 
    padAll 1 $
    vBox [ str "Please enter your Polygon API key:"
         , str apiKey
         ]

drawStockSymbolEntry :: String -> Widget Name
drawStockSymbolEntry symbol = 
    centerLayer $ 
    borderWithLabel (str "Enter Stock Symbol") $ 
    padAll 1 $
    vBox [ str "Please enter the stock symbol (e.g., AAPL):"
         , str symbol
         ]

drawMainUI :: UIState -> Widget Name
drawMainUI s@UIState{..} = 
    borderWithLabel (str $ "Stock Market Simulator - " ++ stockSymbol) $ 
    vBox [ maybe emptyWidget drawStockInfo simState
         , hBorder
         , maybe emptyWidget drawStockGraph simState
         , hBorder
         , drawPortfolio s
         , hBorder
         , drawControls s
         , maybe emptyWidget (str . ("Error: " ++)) errorMessage
         ]

drawStockInfo :: SimulationState -> Widget Name
drawStockInfo SimulationState{..} = 
    let day = stockData !! currentDay
    in vBox
        [ str $ "Day " ++ show (currentDay + 1) ++ " - " ++ T.unpack (date day)
        , str $ Simulation.formatStockData day
        ]

drawStockGraph :: SimulationState -> Widget Name
drawStockGraph SimulationState{..} =
    let graphWidth = 100
        graphHeight = 20
        allData = take (currentDay + 1) stockData
        dataToShow = 
            if length allData <= graphWidth
            then allData
            else drop (length allData - graphWidth) allData
        startDate = date $ head dataToShow
        endDate = date $ last dataToShow
    in vBox
        [ str $ "Stock Price (" ++ T.unpack startDate ++ " to " ++ T.unpack endDate ++ ")"
        , vLimit graphHeight $ hLimit graphWidth $ raw $ Simulation.createLineGraph graphWidth graphHeight dataToShow
        ]

drawPortfolio :: UIState -> Widget Name
drawPortfolio UIState{..} = vBox
    [ str $ "Portfolio Value: $" ++ printf "%.2f" portfolioValue
    , str $ "Cash: $" ++ printf "%.2f" cashAmount
    , str $ "Shares: " ++ show stockShares
    ]


drawControls :: UIState -> Widget Name
drawControls UIState{..} = vBox
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
        (ApiKeyEntry, V.KEnter) -> 
            put $ s { uiMode = StockSymbolEntry }
        (StockSymbolEntry, V.KEnter) -> 
            liftIO (fetchStockData s) >>= put
        (NormalMode, V.KChar 'b') -> put $ s { uiMode = BuyMode, buyQuantity = "" }
        (NormalMode, V.KChar 's') -> put $ s { uiMode = SellMode, sellQuantity = "" }
        (NormalMode, V.KChar 'n') -> liftIO (Simulation.handleNextDay s) >>= put
        (NormalMode, V.KChar 'q') -> halt
        (BuyMode, V.KEnter) -> liftIO (Simulation.handleBuy s) >>= put
        (SellMode, V.KEnter) -> liftIO (Simulation.handleSell s) >>= put
        (ApiKeyEntry, V.KChar c) -> put $ s { apiKey = apiKey s ++ [c] }
        (StockSymbolEntry, V.KChar c) -> put $ s { stockSymbol = stockSymbol s ++ [c] }
        (BuyMode, V.KChar c) -> put $ s { buyQuantity = buyQuantity s ++ [c] }
        (SellMode, V.KChar c) -> put $ s { sellQuantity = sellQuantity s ++ [c] }
        (_, V.KBS) -> put $ s { apiKey = drop 1 (apiKey s)
                              , stockSymbol = drop 1 (stockSymbol s)
                              , buyQuantity = drop 1 (buyQuantity s)
                              , sellQuantity = drop 1 (sellQuantity s)
                              }
        (_, V.KEsc) -> put $ s { uiMode = NormalMode, buyQuantity = "", sellQuantity = "" }
        _ -> return ()
handleEvent _ = return ()

fetchStockData :: UIState -> IO UIState
fetchStockData s = do
    let startDate = "2023-01-01"
        endDate = "2023-12-31"
    result <- StockData.fetchStockData (stockSymbol s) startDate endDate (apiKey s)
    case result of
        Left err -> return $ s { errorMessage = Just $ "Polygon API Error: " ++ err }
        Right stockData ->
            if null stockData
            then return $ s { errorMessage = Just "No data available for the given stock symbol." }
            else do
                portfolioTVar <- newTVarIO []
                cashTVar <- newTVarIO 10000.0
                let simState = SimulationState
                        { currentDay = 0
                        , portfolio = portfolioTVar
                        , cash = cashTVar
                        , stockData = stockData
                        }
                Simulation.updateUIState $ s { simState = Just simState, uiMode = NormalMode, errorMessage = Nothing }

runUI :: IO ()
runUI = do
    let initialState = UIState
            { simState = Nothing
            , uiMode = ApiKeyEntry
            , apiKey = ""
            , stockSymbol = ""
            , buyQuantity = ""
            , sellQuantity = ""
            , portfolioValue = 0
            , cashAmount = 0
            , stockShares = 0
            , errorMessage = Nothing
            }
    void $ defaultMain app initialState

app :: App UIState e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr []
    }
