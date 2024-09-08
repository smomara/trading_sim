{-# LANGUAGE RecordWildCards #-}

module Simulation
    ( createLineGraph
    , formatStockData
    , handleNextDay
    , handleBuy
    , handleSell
    , updateUIState
    , getStockData
    , getCurrentDay
    ) where

import Types
import Control.Concurrent.STM
import Data.List (find)
import Text.Printf (printf)
import qualified Graphics.Vty as V
import qualified Data.Text as T

createLineGraph :: Int -> Int -> [StockData] -> V.Image
createLineGraph width height data'
    | null data' = V.string V.defAttr "No data available"
    | otherwise = V.vertCat $ map (V.string V.defAttr) graphLines
  where
    prices = map close data'
    minPrice = minimum prices
    maxPrice = maximum prices
    priceRange = maxPrice - minPrice
    
    scalePrice :: Double -> Int
    scalePrice price = 
        round $ (fromIntegral (height - 1)) * (price - minPrice) / priceRange
    
    scaledPrices = map scalePrice prices
    
    graphLines = [[if shouldDrawPoint x y then '*' else ' ' | x <- [0..width-1]] | y <- [0..height-1]]
    
    shouldDrawPoint x y =
        let dataIndex = x * (length scaledPrices - 1) `div` (width - 1)
            currentPrice = scaledPrices !! dataIndex
        in y == (height - 1 - currentPrice)

updatePortfolio :: [(String, Int)] -> String -> Int -> [(String, Int)]
updatePortfolio [] symbol quantity = [(symbol, quantity)]
updatePortfolio ((sym, qty):rest) symbol quantity
    | sym == symbol = (sym, qty + quantity) : rest
    | otherwise = (sym, qty) : updatePortfolio rest symbol quantity

formatStockData :: StockData -> String
formatStockData StockData{..} = 
    printf "%s | Open: %.2f | High: %.2f | Low: %.2f | Close: %.2f | Volume: %.0f" 
        (T.unpack date) open high low close volume

handleNextDay :: UIState -> IO UIState
handleNextDay s = do
    let newDay = currentDay (simState s) + 1
    if newDay < length (stockData (simState s))
        then updateUIState $ s { simState = (simState s) { currentDay = newDay } }
        else return s  -- TODO handle end of year

handleBuy :: UIState -> IO UIState
handleBuy s = do
    let quantity = read (buyQuantity s) :: Int
        day = stockData (simState s) !! currentDay (simState s)
    result <- atomically $ do
        cashAmount <- readTVar (cash (simState s))
        if cashAmount < fromIntegral quantity * close day
            then return $ Left ("Insufficient funds" :: String)
            else do
                modifyTVar (cash (simState s)) (\c -> c - fromIntegral quantity * close day)
                modifyTVar (portfolio (simState s)) (\p -> updatePortfolio p "AAPL" quantity)
                return $ Right ()
    case result of
        Left _ -> return s
        Right _ -> updateUIState $ s { uiMode = NormalMode, buyQuantity = "" }

handleSell :: UIState -> IO UIState
handleSell s = do
    let quantity = read (sellQuantity s) :: Int
        day = stockData (simState s) !! currentDay (simState s)
    result <- atomically $ do
        holdings <- readTVar (portfolio (simState s))
        case find (\(sym, _) -> sym == "AAPL") holdings of
            Just (_, ownedQuantity) | ownedQuantity >= quantity -> do
                modifyTVar (cash (simState s)) (\c -> c + fromIntegral quantity * close day)
                modifyTVar (portfolio (simState s)) (\p -> updatePortfolio p "AAPL" (-quantity))
                return $ Right ()
            _ -> return $ Left ("Insufficient stocks to sell" :: String)
    case result of
        Left _ -> return s
        Right _ -> updateUIState $ s { uiMode = NormalMode, sellQuantity = "" }

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

getStockData :: SimulationState -> [StockData]
getStockData = stockData

getCurrentDay :: SimulationState -> Int
getCurrentDay = currentDay
