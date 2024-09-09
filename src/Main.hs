{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Control.Monad (when)
import System.IO (hFlush, stdout)

import Types
import StockData (fetchStockData)
import Simulation

displayState :: SimulationState -> IO ()
displayState SimulationState{..} = do
    let StockData{..} = stockData !! currentDay
    putStrLn $ "Day: " ++ show (currentDay + 1)
    putStrLn $ "Date: " ++ show date
    putStrLn $ "Current Price: $" ++ show close
    putStrLn $ "Cash: $" ++ show cash
    putStrLn $ "Shares: " ++ show shares
    putStrLn $ "Portfolio Value: $" ++ show (cash + fromIntegral shares * close)

promptAction :: IO String
promptAction = do
    putStrLn "\nWhat would you like to do?"
    putStrLn "1. Buy shares"
    putStrLn "2. Sell shares"
    putStrLn "3. Move to next day"
    putStrLn "4. Quit"
    putStr "Enter your choice (1-4): "
    hFlush stdout
    getLine

runSimulation :: SimulationState -> IO ()
runSimulation state = do
    displayState state
    action <- promptAction
    case action of
        "1" -> do
            putStr "Enter number of shares to buy: "
            hFlush stdout
            input <- getLine
            case readMaybe input of
                Just quantity -> runSimulation $ buyShares state quantity
                Nothing -> putStrLn "Invalid input" >> runSimulation state
        "2" -> do
            putStr "Enter number of shares to sell: "
            hFlush stdout
            input <- getLine
            case readMaybe input of
                Just quantity -> runSimulation $ sellShares state quantity
                Nothing -> putStrLn "Invalid input" >> runSimulation state
        "3" -> runSimulation $ nextDay state
        "4" -> putStrLn "Simulation ended."
        _   -> putStrLn "Invalid choice" >> runSimulation state

main :: IO ()
main = do
    putStrLn "Welcome to the Stock Market Simulator!"
    putStr "Enter your Polygon API key: "
    hFlush stdout
    apiKey <- TIO.getLine
    putStr "Enter the stock symbol (e.g., AAPL): "
    hFlush stdout
    symbol <- TIO.getLine
    putStrLn "Fetching stock data..."
    
    result <- fetchStockData symbol "2023-01-01" "2023-12-31" apiKey
    case result of
        Left err -> putStrLn $ "Error fetching stock data: " ++ err
        Right stockData -> do
            if null stockData
                then putStrLn "No stock data retrieved. Please check your inputs and try again."
                else do
                    putStrLn $ "Fetched " ++ show (length stockData) ++ " days of stock data."
                    runSimulation $ initSimulation stockData
