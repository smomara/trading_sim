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
    , generateIntradayPrices
    ) where

import Types
import Control.Concurrent.STM
import Data.List (find, scanl')
import Text.Printf (printf)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import System.Random (RandomGen, random)

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

handleNextDay :: RandomGen g => g -> UIState -> IO (UIState, g)
handleNextDay gen s = case simState s of
    Nothing -> return (s, gen)
    Just sim -> do
        let newDay = currentDay sim + 1
        if newDay < length (stockData sim)
            then do
                let yesterday = stockData sim !! (newDay - 1)
                    today = stockData sim !! newDay
                    yesterdayClose = close yesterday
                    todayClose = close today
                    (intradayPrices, newGen) = generateIntradayPrices gen yesterdayClose todayClose 100  -- Generate 100 intraday points
                    updatedSim = sim { currentDay = newDay, intradayPrices = intradayPrices }
                updatedState <- updateUIState $ s { simState = Just updatedSim }
                return (updatedState, newGen)
            else return (s, gen)  -- End of data reached

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

-- Generate intraday prices using a random walk
generateIntradayPrices :: RandomGen g => g -> Double -> Double -> Int -> ([Double], g)
generateIntradayPrices gen yesterdayClose todayClose numPoints =
    let
        totalChange = todayClose - yesterdayClose
        avgStepSize = totalChange / fromIntegral numPoints
        (randomWalk, newGen) = genRandomWalk gen (numPoints - 1) avgStepSize
        cumulativeChanges = scanl' (+) 0 randomWalk
        scaleFactor = totalChange / (last cumulativeChanges + 1e-10)  -- Avoid division by zero
        prices = map (\change -> yesterdayClose + change * scaleFactor) (0 : cumulativeChanges)
    in
        (prices, newGen)

-- Generate a random walk
genRandomWalk :: RandomGen g => g -> Int -> Double -> ([Double], g)
genRandomWalk gen steps avgStepSize = 
    let
        (randomNumbers, newGen) = genRandomNumbers gen steps
        randomWalk = map (\r -> (r - 0.5) * 2 * avgStepSize) randomNumbers
    in
        (randomWalk, newGen)

-- Generate a list of random numbers between 0 and 1
genRandomNumbers :: RandomGen g => g -> Int -> ([Double], g)
genRandomNumbers gen n =
    let
        (value, newGen) = random gen
        (restValues, finalGen) = genRandomNumbers newGen (n - 1)
    in
        if n <= 0
        then ([], gen)
        else (value : restValues, finalGen)
