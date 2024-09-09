{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Types

initSimulation :: [StockData] -> SimulationState
initSimulation stockData = SimulationState
    { currentDay = 0
    , cash = 10000.0
    , shares = 0
    , stockData = stockData
    }

buyShares :: SimulationState -> Int -> SimulationState
buyShares state@SimulationState{..} quantity =
    let currentPrice = close $ stockData !! currentDay
        cost = fromIntegral quantity * currentPrice
    in if cost <= cash
       then state { cash = cash - cost, shares = shares + quantity }
       else state  -- Not enough cash, state remains unchanged

sellShares :: SimulationState -> Int -> SimulationState
sellShares state@SimulationState{..} quantity =
    let currentPrice = close $ stockData !! currentDay
        revenue = fromIntegral quantity * currentPrice
    in if quantity <= shares
       then state { cash = cash + revenue, shares = shares - quantity }
       else state  -- Not enough shares, state remains unchanged

nextDay :: SimulationState -> SimulationState
nextDay state@SimulationState{..} =
    if currentDay < length stockData - 1
    then state { currentDay = currentDay + 1 }
    else state  -- End of simulation reached
