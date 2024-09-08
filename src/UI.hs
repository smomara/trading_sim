{-# LANGUAGE RecordWildCards #-}

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
import Brick
import Brick.Widgets.Border
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import qualified Data.Text as T

drawUI :: UIState -> [Widget Name]
drawUI UIState{..} = [ui]
  where
    ui = borderWithLabel (str "Stock Market Simulator") $ 
            vBox [ drawStockInfo
                 , hBorder
                 , drawStockGraph
                 , hBorder
                 , drawPortfolio
                 , hBorder
                 , drawControls
                 ]
    
    drawStockInfo = 
        let day = Simulation.getStockData simState !! Simulation.getCurrentDay simState
        in vBox
            [ str $ "Day " ++ show (Simulation.getCurrentDay simState + 1) ++ " - " ++ T.unpack (date day)
            , str $ Simulation.formatStockData day
            ]
    
    drawStockGraph =
        let graphWidth = 100
            graphHeight = 20
            allData = take (Simulation.getCurrentDay simState + 1) $ Simulation.getStockData simState
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
        (NormalMode, V.KChar 'n') -> liftIO (Simulation.handleNextDay s) >>= put
        (NormalMode, V.KChar 'q') -> halt
        (BuyMode, V.KEnter) -> liftIO (Simulation.handleBuy s) >>= put
        (SellMode, V.KEnter) -> liftIO (Simulation.handleSell s) >>= put
        (BuyMode, V.KChar c) -> put $ s { buyQuantity = buyQuantity s ++ [c] }
        (SellMode, V.KChar c) -> put $ s { sellQuantity = sellQuantity s ++ [c] }
        (BuyMode, V.KBS) -> put $ s { buyQuantity = take (length (buyQuantity s) - 1) (buyQuantity s) }
        (SellMode, V.KBS) -> put $ s { sellQuantity = take (length (sellQuantity s) - 1) (sellQuantity s) }
        (_, V.KEsc) -> put $ s { uiMode = NormalMode, buyQuantity = "", sellQuantity = "" }
        _ -> return ()
handleEvent _ = return ()

runUI :: UIState -> IO UIState
runUI initialState = defaultMain app initialState

app :: App UIState e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr []
    }
