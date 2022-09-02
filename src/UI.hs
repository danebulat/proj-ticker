{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ?~" #-}

module UI where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (newTChanIO)
import Control.Concurrent.STM.TChan (TChan)
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center
import qualified Brick.BChan as BC
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as BT

import Data.Default
import Data.Maybe (fromJust)
import Control.Lens.TH
import Control.Lens ((&), (^.), (.~), (.=), (%=))
import Lens.Micro.Mtl (use)

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
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

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent e =
  case e of
    -- Custom app events
    AppEvent (CacheConnection c) -> conn .= Just c

    AppEvent (CacheThreadId t) -> do
      threads %= \ts -> t : ts

    AppEvent (TickerMessage t) -> do
      tickers %= \ts -> rebuildTickers t ts

    AppEvent ErrorMessage -> return ()
    AppEvent ResponseMessage -> return ()

    -- Tab keys (change focus)
    VtyEvent (V.EvKey (V.KChar '\t') []) ->
      focusRing %= F.focusNext

    VtyEvent (V.EvKey V.KBackTab []) ->
      focusRing %= F.focusPrev

    -- For later
    VtyEvent (V.EvKey V.KEnter []) -> return ()

    -- Exit program
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt

    -- Handle editor widget
    ev -> do
      r <- use focusRing
      case F.focusGetCurrent r of
        Just Edit1 -> zoom edit1 $ E.handleEditorEvent ev
        Nothing    -> return ()
        _          -> return ()

-- -------------------------------------------------------------------
-- UI

brickApp :: App AppState CustomEvent Name
brickApp = App { appDraw         = drawUI
               , appChooseCursor = appCursor
               , appHandleEvent  = appEvent
               , appStartEvent   = return ()
               , appAttrMap      = const theMap
               }

-- draw UI
drawUI :: AppState -> [Widget Name]
drawUI s = [ui s]

ui :: AppState -> Widget Name
ui st = if null (st ^. tickers)
         then center $ txt "connecting..."
         else vBox [ C.hCenter ( padTop (Pad 4)
                               $ padBottom (Pad 2)
                               $ drawEditor st
                             <+> drawButtons)
                 <=> drawTable st ]

-- edit box
drawEditor :: AppState -> Widget Name
drawEditor st = txt "Symbol Pair: " <+> hLimit 20 (vLimit 1 e1)
  where
    e1 = F.withFocusRing (st ^. focusRing)
                         (E.renderEditor (txt . T.unlines))
                         (st ^. edit1)

-- buttons
drawButtons :: Widget Name
drawButtons = hBox $ padLeftRight 1 <$> buttons
  where buttons = mkButton <$> buttonData
        buttonData = [ ("F2 ", "Add",    attrName "addButton")
                     , ("F3 ", "Remove", attrName "removeButton")
                     ]
        -- pass Name value to make button clickable
        mkButton (key, label, attr) =
          txt key <+>
          withDefAttr attr (padLeftRight 1 $ txt label)
          --  $ B.border
          --  $ padTopBottom 1

-- ticker table
drawTable :: AppState -> Widget Name
drawTable st =
  C.hCenter $ renderTable $ table $ drawRows ts
  where
    ts = st ^. tickers
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
-- App Cursor 
-- https://hackage.haskell.org/package/brick-1.1/docs/Brick-Focus.html#v:focusRingCursor

appCursor :: AppState -> [BT.CursorLocation Name] -> Maybe (BT.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

-- -------------------------------------------------------------------
-- Attribute Map

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "plusTicker",   fg V.brightGreen)
  , (attrName "minusTicker",  fg V.brightRed)
  , (attrName "addButton",    V.black `on` V.cyan)
  , (attrName "removeButton", V.black `on` V.cyan)
  , (E.editAttr,              V.white `on` V.blue)
  , (E.editFocusedAttr,       V.white `on` V.blue)
  ]

-- -------------------------------------------------------------------
-- Main

initialState :: TChan ServerRequest -> AppState
initialState reqChan =
  AppState
  { _tickers   = []
  , _conn      = Nothing
  , _threads   = []
  , _reqChan   = Just reqChan
  , _focusRing = F.focusRing [Edit1]
  , _edit1     = E.editor Edit1 (Just 1) ""
  }

main :: IO ()
main = do
  requestChannel <- newTChanIO
  eventChan <- BC.newBChan 10

  -- default state
  let s = initialState requestChannel

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

