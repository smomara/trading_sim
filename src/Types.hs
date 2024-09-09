{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Text (Text)
import Data.Time (Day)
import Data.Aeson
import Control.Monad (mzero)

data StockData = StockData
    { date   :: Day
    , open   :: Double
    , high   :: Double
    , low    :: Double
    , close  :: Double
    , volume :: Double
    } deriving (Show)

instance FromJSON StockData where
    parseJSON (Object v) = StockData 
        <$> v .: "date"
        <*> v .: "open"
        <*> v .: "high"
        <*> v .: "low"
        <*> v .: "close"
        <*> v .: "volume"
    parseJSON _ = mzero

data SimulationState = SimulationState
    { currentDay :: Int
    , cash       :: Double
    , shares     :: Int
    , stockData  :: [StockData]
    } deriving (Show)
