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

removeTickerWithSymbol :: Text -> [Ticker] -> [Ticker]
removeTickerWithSymbol x = filter (\t -> (t^.tckSymbolPair) /= symUpper)
  where symUpper = T.toUpper x


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

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent e =
  case e of
    -- Custom app events
    AppEvent (CacheConnection c) -> conn .= Just c

    AppEvent (CacheThreadId t) -> do
      threads %= \ts -> t : ts

    AppEvent (TickerMessage t) -> do
      tickers %= \ts -> rebuildTickers t ts

    AppEvent (ErrorMessage err) -> do
      processingReq .= False
      statusText .= (T.pack . show $ err)

    AppEvent (ResponseMessage res) -> do
      processingReq .= False
      statusText .= (T.pack . show $ res)

    -- Tab keys (change focus)
    VtyEvent (V.EvKey (V.KChar '\t') []) ->
      focusRing %= F.focusNext

    VtyEvent (V.EvKey V.KBackTab []) ->
      focusRing %= F.focusPrev

    -- F2
    VtyEvent (V.EvKey (V.KFun 2) []) -> do
      pr <- use processingReq
      if pr
        then statusText .= "error - a request is already being handled"
        else do
          e1 <- use edit1
          ts <- use tickers
          let contents = E.getEditContents e1

          -- check if contents is an existing ticker
          if searchTickerSymbol (head contents) ts
            then statusText .= head contents `T.append` " - already added!"
            else do
              -- validate symbol
              if validateSymbol (head contents)
                then do processingReq .= True
                        -- write request
                        chan <- use reqChan
                        liftIO $ Sockets.writeRequests contents "SUBSCRIBE" (fromJust chan)
                        statusText .= head contents `T.append` " request sent"
                else statusText .= head contents
                       `T.append` " - validation failed! Enter a valid symbol."

    -- F3
    VtyEvent (V.EvKey (V.KFun 3) []) -> do
      pr <- use processingReq
      if pr
        then statusText .= "error - a request is already being handled"
        else do
          e1 <- use edit1
          ts <- use tickers
          let contents = E.getEditContents e1

          -- check if text is an existing ticker
          if searchTickerSymbol (head contents) ts
            then do processingReq .= True
                    let symbolLower = T.toLower (head contents)
                    chan <- use reqChan
                    liftIO $ Sockets.writeRequests [symbolLower] "UNSUBSCRIBE" (fromJust chan)
                    tickers %= removeTickerWithSymbol (head contents)
                    statusText .= head contents `T.append` "removed"
            else statusText .= head contents `T.append` " - not in current ticker list"

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
         else
           vBox [hBox [ vBox [ C.hCenterLayer ( padTop (Pad 4)
                $ padBottom (Pad 2)
                $ drawEditor st
              <+> drawButtons st)
            ] ]
            , padLeftRight 2 (drawTable st)
            , drawInfoLayer st
            ]

-- draw responsive table
drawTable :: AppState -> Widget Name
drawTable st =
    vBox [ B.hBorder
         , mkHeaders
         , B.hBorder ] <=> drawRows ts

  where headers   = ["Symbols", "Open", "Close", "High", "Low", "Volume", "Traded"]
        cols      = map (padRight Max . txt) headers
        mkHeaders = hBox cols
        ts        = st ^. tickers

-- draw table rows
drawRows :: [Ticker] -> Widget Name
drawRows = foldr (\x acc ->
    let cs  = colText x
        cs' = map (padRight Max . txt) cs
    in acc <=> vBox [ hBox cs', B.hBorder ]) emptyHBox
  where
    emptyHBox = hBox []
    colText t = [ t^.tckSymbolPair
                , fmtPicoText 4 (t^.tckOpen)
                , fmtPicoText 4 (t^.tckClose)
                , fmtPicoText 4 (t^.tckHigh)
                , fmtPicoText 4 (t^.tckLow)
                , fmtPicoText 2 (t^.tckVolume)
                , fmtPicoText 2 (t^.tckTrades)
                ]

-- edit box
drawEditor :: AppState -> Widget Name
drawEditor st = txt "Symbol Pair: " <+> hLimit 20 (vLimit 1 e1)
  where
    e1 = F.withFocusRing (st ^. focusRing)
                         (E.renderEditor (txt . T.unlines))
                         (st ^. edit1)

-- buttons
drawButtons :: AppState -> Widget Name
drawButtons st = hBox $ padLeftRight 1 <$> buttons
  where buttons = mkButton <$> buttonData
        buttonData = [ ("F2 ", "Add",    attrName buttonAttr)
                     , ("F3 ", "Remove", attrName buttonAttr)
                     ]
        buttonAttr = if st^.processingReq
                       then "buttonDisable"
                       else "buttonEnable"
        -- pass Name value to make button clickable
        mkButton (key, label, attr) =
          txt key <+>
          withDefAttr attr (padLeftRight 1 $ txt label)
          --  $ B.border
          --  $ padTopBottom 1

-- draw status "info" bar
drawInfoLayer :: AppState -> Widget Name
drawInfoLayer st = Widget Fixed Fixed $ do
  c <- getContext
  let h = c ^. availHeightL
      msg = st ^. statusText
  render $ translateBy (Location (0, h-1))
         $ withDefAttr (attrName "info")
         $ C.hCenter
         $ txt msg

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
  [ (attrName "plusTicker",    fg V.brightGreen)
  , (attrName "minusTicker",   fg V.brightRed)
  , (attrName "buttonEnable",  V.black `on` V.cyan)
  , (attrName "buttonDisable", V.black `on` V.color240 77 77 77)
  , (attrName "info",          V.white `on` V.color240 64 0 128)
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

