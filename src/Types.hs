{-# LANGUAGE OverloadedStrings #-}

module Types
    ( StockData(..)
    , SimulationState(..)
    , UIState(..)
    , UIMode(..)
    , Name(..)
    ) where

import Data.Text (Text)
import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

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

data SimulationState = SimulationState
    { currentDay :: Int
    , portfolio :: TVar [(String, Int)]
    , cash :: TVar Double
    , stockData :: [StockData]
    , intradayPrices :: [Double]
    }

data UIState = UIState
    { simState :: Maybe SimulationState
    , uiMode :: UIMode
    , apiKey :: String
    , stockSymbol :: String
    , buyQuantity :: String
    , sellQuantity :: String
    , portfolioValue :: Double
    , cashAmount :: Double
    , stockShares :: Int
    , errorMessage :: Maybe String
    }

data UIMode = ApiKeyEntry | StockSymbolEntry | NormalMode | BuyMode | SellMode
    deriving (Eq, Show)

data Name = StockList
    deriving (Ord, Eq, Show)
