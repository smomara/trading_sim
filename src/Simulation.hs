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
handleNextDay s = case simState s of
    Nothing -> return s
    Just sim -> do
        let newDay = currentDay sim + 1
        if newDay < length (stockData sim)
            then updateUIState $ s { simState = Just sim { currentDay = newDay } }
            else return s -- TODO handle end of year

handleBuy :: UIState -> IO UIState
handleBuy s = case simState s of
    Nothing -> return s
    Just sim -> do
        let quantity = read (buyQuantity s) :: Int
            day = stockData sim !! currentDay sim
        result <- atomically $ do
            cashAmount <- readTVar (cash sim)
            if cashAmount < fromIntegral quantity * close day
                then return $ Left ("Insufficient funds" :: String)
                else do
                    modifyTVar (cash sim) (\c -> c - fromIntegral quantity * close day)
                    modifyTVar (portfolio sim) (\p -> updatePortfolio p (stockSymbol s) quantity)
                    return $ Right ()
        case result of
            Left err -> return $ s { errorMessage = Just err }
            Right _ -> updateUIState $ s { uiMode = NormalMode, buyQuantity = "" }

handleSell :: UIState -> IO UIState
handleSell s = case simState s of
    Nothing -> return s
    Just sim -> do
        let quantity = read (sellQuantity s) :: Int
            day = stockData sim !! currentDay sim
        result <- atomically $ do
            holdings <- readTVar (portfolio sim)
            case find (\(sym, _) -> sym == stockSymbol s) holdings of
                Just (_, ownedQuantity) | ownedQuantity >= quantity -> do
                    modifyTVar (cash sim) (\c -> c + fromIntegral quantity * close day)
                    modifyTVar (portfolio sim) (\p -> updatePortfolio p (stockSymbol s) (-quantity))
                    return $ Right ()
                _ -> return $ Left ("Insufficient stocks to sell" :: String)
        case result of
            Left err -> return $ s { errorMessage = Just err }
            Right _ -> updateUIState $ s { uiMode = NormalMode, sellQuantity = "" }

updateUIState :: UIState -> IO UIState
updateUIState s = case simState s of
    Nothing -> return s
    Just sim -> do
        let day = stockData sim !! currentDay sim
        (portfolioValue, cashAmount, stockShares) <- atomically $ do
            holdings <- readTVar (portfolio sim)
            cash <- readTVar (cash sim)
            let stockShares = sum [qty | (sym, qty) <- holdings, sym == stockSymbol s]
                stockValue = fromIntegral stockShares * close day
            return (cash + stockValue, cash, stockShares)
        return $ s { portfolioValue = portfolioValue
                   , cashAmount = cashAmount
                   , stockShares = stockShares 
                   }

getStockData :: SimulationState -> [StockData]
getStockData = stockData

getCurrentDay :: SimulationState -> Int
getCurrentDay = currentDay
