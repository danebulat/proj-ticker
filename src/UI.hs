{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Control.Concurrent (ThreadId)

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center
import Brick.BChan
import Data.Default
import Data.Maybe (fromJust)
import Control.Lens.TH
import Control.Lens ((&), (^.), (.~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Network.WebSockets as WS
import qualified Graphics.Vty as V
import qualified Brick.BChan as Brick

import Format
import WebTypes
import UITypes
import Sockets

-- -------------------------------------------------------------------
-- Event Handler

handleEvent :: AppState -> BrickEvent () CustomEvent -> EventM () (Next AppState)

-- custom events
handleEvent s (AppEvent (CacheConnection c)) =
  continue $ s & conn .~ Just c
  
handleEvent s (AppEvent (CacheThreadId t)) = do
  let ts = s ^. threads
  continue $ s & threads .~ (t : ts)

handleEvent s (AppEvent ErrorMessage)      = continue s
handleEvent s (AppEvent ResponseMessage)   = continue s
handleEvent s (AppEvent (TickerMessage t)) = do
  -- update ticker
  continue $ s & ticker .~ t

-- start connection
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = do continue s

-- quit app
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s _                                     = continue s

-- -------------------------------------------------------------------
-- UI

brickApp :: App AppState CustomEvent ()
brickApp = App { appDraw         = drawUI
               , appChooseCursor = neverShowCursor
               , appHandleEvent  = handleEvent
               , appStartEvent   = return
               , appAttrMap      = const theMap
               }

-- draw UI
drawUI :: AppState -> [Widget ()]
drawUI s = [ui s]

ui :: AppState -> Widget ()
ui s = center $ renderTable (mainTable s)

mainTable :: AppState -> Table ()
mainTable s = table [
  [ txt $ t ^. tckSymbolPair
  , txt $ "O: " <^> fmtPicoText 4 (t ^. tckOpen)
  , txt $ "C: " <^> fmtPicoText 4 (t ^. tckClose)
  , txt $ "H: " <^> fmtPicoText 4 (t ^. tckHigh)
  , txt $ "L: " <^> fmtPicoText 4 (t ^. tckLow)
  , txt $ "V: " <^> fmtPicoText 2 (t ^. tckVolume)
  , txt $ "T: " <^> fmtPicoText 2 (t ^. tckTrades)
  ] ]
  where
    t = s ^. ticker

-- -------------------------------------------------------------------
-- Attribute Map

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (plusTickerAttr,  fg V.brightGreen)
  , (minusTickerAttr, fg V.brightRed)
  ]

plusTickerAttr :: AttrName
plusTickerAttr = "plusTicker"

minusTickerAttr :: AttrName
minusTickerAttr = "minusTicker"

-- -------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 10

  -- spawn threads here
  connectBinance eventChan

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
    (Just eventChan) brickApp def

  -- safely kill threads
  killThreads $ finalState ^. threads
  print "> killed threads"

  -- safely close connection
  WS.sendClose (fromJust $ finalState ^. conn) (pack "Bye!")
  print "> killed connection"
