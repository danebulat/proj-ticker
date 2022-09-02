{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (newTChanIO)
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center
import qualified Brick.BChan as BC
import Data.Default
import Data.Maybe (fromJust)
import Control.Lens.TH
import Control.Lens ((&), (^.), (.~), (.=), (%=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Map (Map)
import qualified Network.WebSockets as WS
import qualified Graphics.Vty as V

import Format
import WebTypes
import UITypes
import Sockets

-- -------------------------------------------------------------------
-- Event Handler

searchTicker :: Ticker -> [Ticker] -> Bool
searchTicker t = foldr (\x acc ->
    if acc then acc else x ^. tckSymbolPair == target) False
  where target = t ^. tckSymbolPair

rebuildTickers :: Ticker -> [Ticker] -> [Ticker]
rebuildTickers t ts
  | null ts = [t]
  | searchTicker t ts = map (\x -> if x ^. tckSymbolPair == target then t else x) ts
  | otherwise = t : ts
  where
    target = t ^. tckSymbolPair

appEvent :: BrickEvent () CustomEvent -> EventM () AppState ()
appEvent e =
  case e of
    AppEvent (CacheConnection c) -> conn .= Just c

    AppEvent (CacheThreadId t) -> do
      threads %= \ts -> t : ts

    AppEvent (TickerMessage t) -> do
      tickers %= \ts -> rebuildTickers t ts

    AppEvent ErrorMessage -> return ()
    AppEvent ResponseMessage -> return ()

    -- For later
    VtyEvent (V.EvKey V.KEnter []) -> return ()

    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
    _ -> return ()

-- -------------------------------------------------------------------
-- UI

brickApp :: App AppState CustomEvent ()
brickApp = App { appDraw         = drawUI
               , appChooseCursor = neverShowCursor
               , appHandleEvent  = appEvent
               , appStartEvent   = return ()
               , appAttrMap      = const theMap
               }

-- draw UI
drawUI :: AppState -> [Widget ()]
drawUI s = [ui s]

ui :: AppState -> Widget ()
ui s = if null (s ^. tickers)
         then center $ txt "connecting..."
         else center $ renderTable (drawTable $ s ^. tickers)

drawTable :: [Ticker] -> Table ()
drawTable ts =
  table $ drawRows ts
  where
    drawRows [] = []
    drawRows (t:ts) =
       [ txt $ t ^. tckSymbolPair
       , txt $ "O: " <^> fmtPicoText 4 (t ^. tckOpen)
       , txt $ "C: " <^> fmtPicoText 4 (t ^. tckClose)
       , txt $ "H: " <^> fmtPicoText 4 (t ^. tckHigh)
       , txt $ "L: " <^> fmtPicoText 4 (t ^. tckLow)
       , txt $ "V: " <^> fmtPicoText 2 (t ^. tckVolume)
       , txt $ "T: " <^> fmtPicoText 2 (t ^. tckTrades)
       ] : drawRows ts

-- -------------------------------------------------------------------
-- Attribute Map

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "plusTicker",  fg V.brightGreen)
  , (attrName "minusTicker", fg V.brightRed)
  ]

-- -------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  requestChannel  <- newTChanIO
  eventChan <- BC.newBChan 10

  -- default state
  let s = def & reqChan .~ Just requestChannel

  -- spawn threads here
  connectBinance eventChan (fromJust $ s ^. reqChan)

  -- run ui
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
    (Just eventChan) brickApp s

  -- safely kill threads
  killThreads $ finalState ^. threads
  print "> killed threads"

  -- safely close connection
  WS.sendClose (fromJust $ finalState ^. conn) (pack "Bye!")
  print "> killed connection"

