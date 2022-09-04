{-# LANGUAGE OverloadedStrings #-}
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
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Control.Lens.TH
import Control.Lens ((&), (^.), (.~), (.=), (%=))
import Lens.Micro.Mtl (use)

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Zipper (clearZipper)
import Data.Map (Map)
import qualified Network.WebSockets as WS
import qualified Graphics.Vty as V
import System.Clock (Clock(Monotonic), getTime, TimeSpec, toNanoSecs)

import Format
import WebTypes
import UITypes
import Sockets
import Graphics.Vty (withStyle)

-- -------------------------------------------------------------------
-- Functions called in event handlers

-- takes a symbol and checks if it's in the removal buffer
isInRemoveBuffer :: Text -> [(Text, TimeSpec)] -> Bool
isInRemoveBuffer sym = foldr (\x acc ->
  if acc then acc else T.toUpper sym == fst x) False

-- removes any symbol that the remove buffer has cached for over 10 seconds
updateRemoveBuffer :: TimeSpec -> [(Text, TimeSpec)] -> [(Text, TimeSpec)]
updateRemoveBuffer curTime = filter (go curTime)
  where
    go ct x = let start   = snd x
                  nSecs   = toNanoSecs (ct - start)
                  seconds = fromIntegral nSecs / (10^9)
              in seconds < duration
    duration = 10.0  -- remove symbol from buffer after 10 seconds

-- sort tickers alphabetically using the symbol pair
sortTickerAlpha :: [Ticker] -> [Ticker]
sortTickerAlpha = sortBy (\t1 t2 ->
  compare (t2^.tckSymbolPair) (t1^.tckSymbolPair))

removeTickerWithSymbol :: Text -> [Ticker] -> [Ticker]
removeTickerWithSymbol x = filter (\t -> (t^.tckSymbolPair) /= symUpper)
  where symUpper = T.toUpper x

-- validates editor text when adding a symbol pair
validateSymbol :: Text -> Bool
validateSymbol ts = isAlpha && noSpaces && goodLength
  where validChars = ['A'..'Z'] ++ ['a'..'z']
        isAlpha = foldr (\x acc ->
          if not acc then acc else x `elem` validChars) True (T.unpack ts)
        noSpaces = ' ' `notElem` T.unpack ts
        goodLength = T.length ts > 4

searchTickerSymbol :: Text -> [Ticker] -> Bool
searchTickerSymbol t =
  foldr (\x acc -> if acc then acc else T.toUpper t == x ^. tckSymbolPair) False

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

vp1Scroll :: ViewportScroll Name
vp1Scroll = viewportScroll VP1

-- -------------------------------------------------------------------
-- Event Handler

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent e =
  case e of
    -- cache connection in app state
    AppEvent (CacheConnection c) -> conn .= Just c

    -- cache threadIds in app state
    AppEvent (CacheThreadId t) -> do
      threads %= \ts -> t : ts

    -- handle received table tickers
    AppEvent (TickerMessage t) -> do
      curTime <- liftIO $ getTime Monotonic

      -- update the removal buffer
      removeBuffer %= \rb -> if null rb then rb
        else updateRemoveBuffer curTime rb

      -- update ticker only if its not in the removal buffer
      rb <- use removeBuffer
      if isInRemoveBuffer (t^.tckSymbolPair) rb
        then return ()
        else
          -- sort tickers by symbol pair
          tickers %= \ts -> sortTickerAlpha $ rebuildTickers t ts

    -- wss error received
    AppEvent (ErrorMessage err) -> do
      processingReq .= False
      statusText .= (T.pack . show $ err)

    -- wss response received
    AppEvent (ResponseMessage res) -> do
      processingReq .= False
      statusText .= (T.pack . show $ res)

    -- tab keys (change focus)
    VtyEvent (V.EvKey (V.KChar '\t') []) ->
      focusRing %= F.focusNext

    VtyEvent (V.EvKey V.KBackTab []) ->
      focusRing %= F.focusPrev

    -- handle F2 key press
    VtyEvent (V.EvKey (V.KFun 2) []) -> do
      pr <- use processingReq
      if pr
        then statusText .= "error - a request is already being handled"
        else do
          -- get text from editor
          e1 <- use edit1
          ts <- use tickers
          let contents = E.getEditContents e1

          -- check if contents is an existing ticker
          if searchTickerSymbol (head contents) ts
            then statusText .= head contents `T.append` " is already added!"
            else do
              -- validate symbol
              if not $ validateSymbol (head contents)
                then statusText .= "validation failed! Enter a valid symbol."
                else do
                  -- check if symbol is in removal buffer
                  rb <- use removeBuffer
                  if isInRemoveBuffer (T.toUpper . T.strip . head $ contents) rb
                    then statusText .= "wait a few more seconds to add this symbol again"
                    else do
                      processingReq .= True
                      -- write request
                      chan <- use reqChan
                      liftIO $ Sockets.writeRequests contents "SUBSCRIBE" (fromJust chan)
                      edit1 %= E.applyEdit clearZipper
                      statusText .= head contents `T.append` " request sent"

    -- handle f3 key press
    VtyEvent (V.EvKey (V.KFun 3) []) -> do
      pr <- use processingReq
      if pr
        then statusText .= "error - a request is already being handled"
        else do
          ts <- use tickers
          if length ts < 2
            then statusText .= "error - at least one ticker must be present"
            else do
              e1 <- use edit1      
              let contents = E.getEditContents e1

              -- check if text is an existing ticker
              if searchTickerSymbol (head contents) ts
                then do processingReq .= True
                        let symbolLower = T.toLower (head contents)
                        chan <- use reqChan
                        liftIO $ Sockets.writeRequests [symbolLower] "UNSUBSCRIBE" (fromJust chan)
                        tickers %= removeTickerWithSymbol (head contents)
                        edit1 %= E.applyEdit clearZipper

                        -- add symbol and current time to removal buffer
                        curTime <- liftIO $ getTime Monotonic
                        removeBuffer %= \rb -> (T.toUpper $ head contents, curTime) : rb

                        -- update status line
                        rb <- use removeBuffer
                        statusText .= head contents `T.append` " removed"
                else statusText .= head contents `T.append` " is not in current ticker list"

    -- scroll viewport
    VtyEvent (V.EvKey (V.KFun 4) []) -> vScrollBy vp1Scroll 1

    VtyEvent (V.EvKey (V.KFun 5) []) -> vScrollBy vp1Scroll (-1)

    -- exit program
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt

    -- handle editor widget
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
         else
           vBox [ hBox [ vBox [ C.hCenterLayer (padTop (Pad 2)
                $ padBottom (Pad 2)
                $ drawEditor st
              <+> drawButtons st)
            ] ]
            -- vertical limit set to 80% of terminal height
            , vLimitPercent 80 $ viewport VP1 Vertical $ vBox [ padLeftRight 2 (drawTable st) ]
            , drawInfoLayer st
            ]

-- draw responsive table
drawTable :: AppState -> Widget Name
drawTable st = 
    vBox [ B.hBorder
         , mkHeaders
         , B.hBorder ] <=> drawRows ts

  where headers   = ["Symbols", " 24h%", "Open", "Close", "High", "Low", "Volume", "Traded"]
        cols      = map (withDefAttr (attrName "tableHeader") . padRight Max . txt) headers
        mkHeaders = hBox cols
        ts        = st ^. tickers

-- draw table rows
drawRows :: [Ticker] -> Widget Name
drawRows = foldr (\x acc ->
    let cs  = colText x
        cs' = map (padRight Max) cs
    in acc <=> vBox [ hBox cs', B.hBorder ]) emptyHBox
  where
    emptyHBox = hBox []
    colText t = [ txt $ t^.tckSymbolPair
                , colPct (pct t) . txt $ padPosPct (fmtPicoText 2 (pct t)) `T.append` "%"
                , txt $ fmtPicoText 4 (t^.tckOpen)
                , txt $ fmtPicoText 4 (t^.tckClose)
                , txt $ fmtPicoText 4 (t^.tckHigh)
                , txt $ fmtPicoText 4 (t^.tckLow)
                , txt $ fmtPicoText 2 (t^.tckVolume)
                , txt $ fmtPicoText 2 (t^.tckTrades)
                ]
      where diff t = 100 * (t^.tckClose - t^.tckOpen)
            pct t = diff t / (t^.tckOpen)

            -- style percentage values appropriately
            colPct v
              | abs v < 0.01 = withDefAttr (attrName "neutral")
              | v > 0        = withDefAttr (attrName "plusTicker")
              | otherwise    = withDefAttr (attrName "minusTicker")

            -- prefix space character to positive percentages
            padPosPct t
              | T.head t /= '-' = " " `T.append` t
              | otherwise       = t

-- edit box
drawEditor :: AppState -> Widget Name
drawEditor st = txt "Symbol Pair: " <+> hLimit 20 (vLimit 1 e1)
  where
    e1 = F.withFocusRing (st ^. focusRing)
                         (E.renderEditor (txt . T.unlines))
                         (st ^. edit1)

-- buttons
drawButtons :: AppState -> Widget Name
drawButtons st = padLeft (Pad 1) $ hBox $ padRight (Pad 1) <$> buttons
  where buttons = mkButton <$> buttonData
        buttonData = [ ("F2 ",    "Add",    attrName buttonAttr)
                     , ("F3 ",    "Remove", attrName buttonAttr)
                     , ("F4|F5 ", "Scroll", attrName buttonAttr)
                     ]
        buttonAttr = if st^.processingReq
                       then "buttonDisable"
                       else "buttonEnable"
        -- pass Name value to make button clickable
        mkButton (key, label, attr) =
          txt key <+>
          withDefAttr attr (padLeftRight 1 $ txt label)

-- draw status line
drawInfoLayer :: AppState -> Widget Name
drawInfoLayer st = Widget Fixed Fixed $ do
  c <- getContext
  let h = c ^. availHeightL
      msg = st ^. statusText
  render $ translateBy (Location (0, h-1))
         $ withDefAttr (attrName "info")
         $ C.hCenter
         $ txt msg

-- alternative function to draw status line
drawInfoLayer' :: AppState -> Widget Name
drawInfoLayer' st = withDefAttr (attrName "info")
                  $ hBox [ C.hCenter $ vLimit 1 $ txt msg ]
  where msg = st ^. statusText

-- -------------------------------------------------------------------
-- App Cursor 
-- https://hackage.haskell.org/package/brick-1.1/docs/Brick-Focus.html#v:focusRingCursor

appCursor :: AppState -> [BT.CursorLocation Name] -> Maybe (BT.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

-- -------------------------------------------------------------------
-- Attribute Map

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "neutral",       fg V.brightBlue)
  , (attrName "plusTicker",    fg V.brightGreen)
  , (attrName "minusTicker",   fg V.brightRed)
  , (attrName "buttonEnable",  V.black `on` V.cyan)
  , (attrName "buttonDisable", V.black `on` V.color240 77 77 77)
  , (attrName "info",          V.white `on` V.color240 64 0 128)
  , (attrName "tableHeader",   fg (V.color240 166 166 166)  `withStyle` V.bold)
  , (E.editAttr,               V.white `on` V.blue)
  , (E.editFocusedAttr,        V.white `on` V.blue)
  ]

-- -------------------------------------------------------------------
-- Main

initialState :: TChan ServerRequest -> AppState
initialState reqChan =
  AppState
  { _tickers       = []
  , _conn          = Nothing
  , _threads       = []
  , _reqChan       = Just reqChan
  , _focusRing     = F.focusRing [Edit1]
  , _edit1         = E.editor Edit1 (Just 1) ""
  , _statusText    = "F2/F3 to add/remove symbol pair"
  , _processingReq = False
  , _removeBuffer  = []
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

