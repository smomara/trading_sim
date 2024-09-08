module Main where

import qualified StockData
import qualified UI
import qualified Simulation
import Types
import Control.Monad (void)
import Control.Concurrent.STM

main :: IO ()
main = do
    let symbol = "AAPL"
        startDate = "2023-01-01"
        endDate = "2023-12-31"
        apiKey = "LeHcyWk21YSgaBUxaz9PHevRNnThc7bF"
    result <- StockData.fetchStockData symbol startDate endDate apiKey
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
            initialState <- Simulation.updateUIState UIState
                { simState = simState
                , uiMode = NormalMode
                , buyQuantity = ""
                , sellQuantity = ""
                , portfolioValue = 0
                , cashAmount = 0
                , aapleShares = 0
                }
            void $ UI.runUI initialState
