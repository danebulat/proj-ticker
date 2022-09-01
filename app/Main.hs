{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkIO, ThreadId, throwTo, threadDelay,
                           MVar(..), newMVar, takeMVar, putMVar, newEmptyMVar)
import Control.Monad (forever, unless, void, forM_)
import Data.Maybe (fromJust)
import Data.Aeson (encode, decode)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified System.Random as R

import qualified Network.WebSockets as WS
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, sendPing)
import qualified Wuss
import Control.Exception (AsyncException(ThreadKilled))

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center
import Brick.BChan
import qualified Graphics.Vty as V
import Data.Default
import Control.Lens.TH
import Control.Lens ((&), (^.), (.~))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import WebTypes
import Format
import Control.Lens (makeLenses)
import qualified Graphics.Vty
import qualified Brick.BChan as Brick

data CustomEvent
  = CacheThreadId ThreadId          -- ^ for closing threads safely when app closes 
  | CacheConnection WS.Connection   -- ^ for closing connection safely when app closes 
  | ErrorMessage
  | ResponseMessage
  | TickerMessage Ticker
  | QuitApp


-- -------------------------------------------------------------------
-- Makers 

mkReq :: IO ServerRequest
mkReq = return $ ServerRequest 1 "SUBSCRIBE" ["btcusdt@miniTicker"]

mkReq' :: Int -> Text -> Text -> ServerRequest
mkReq' reqId method params = ServerRequest
  { _srqRequestId = reqId
  , _srqMethod    = method
  , _srqParams    = [params] }

mkReqIO :: IO ServerRequest
mkReqIO = do
  rid <- R.randomRIO (1, 999)
  prompt "> Enter params: "
  params <- getLine
  return $ ServerRequest rid "SUBSCRIBE" [T.strip (T.pack params)]
  where
    prompt p = putStrLn p

-- -------------------------------------------------------------------
-- UI

-- placeholder 
data AppState = AppState
  { _logMsg    :: Text
  , _pairCount :: Int
  , _hasConn   :: Bool
  , _ticker    :: Ticker

  , _conn      :: Maybe WS.Connection
  , _threads   :: [ThreadId]
  }

makeLenses 'AppState

instance Default AppState where
  def = AppState { _logMsg    = "Starting..."
                 , _pairCount = 0
                 , _hasConn   = False
                 , _ticker    = def
                 , _threads   = []
                 , _conn      = Nothing 
                 }

-- named resources 
data Name = VP1 deriving (Ord, Show, Eq)

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

  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
    (Just eventChan) brickApp def

  -- safely kill threads
  killThreads $ finalState ^. threads
  print "> killed threads"

  -- safely close connection
  sendClose (fromJust $ finalState ^. conn) (pack "Bye!")
  print "> killed connection"

-- -------------------------------------------------------------------
-- Web Sockets

-- | Connect to Binance WSS in a child thread
connectBinance :: Brick.BChan.BChan CustomEvent -> IO ()
connectBinance chan = do
  void . forkIO $ Wuss.runSecureClient url port path (wsApp chan)
    where
    url  = "stream.binance.com"
    port = 9443
    path = "/ws"

-- | App within web socket connection context 
wsApp ::  Brick.BChan.BChan CustomEvent -> ClientApp ()
wsApp chan conn = do
  --putStrLn "Connected!"
  t1 <- receiveWs chan conn
  sendWs conn

  -- cache connection and thread ids in app state
  Brick.BChan.writeBChan chan (CacheConnection conn)
  Brick.BChan.writeBChan chan (CacheThreadId t1)

  -- NOTE: THIS THREAD NEEDS TO STAY OPEN
  -- TODO: Implement channels
  threadDelay (10 ^ 6 * 60 * 60) -- keep alive for an hour

-- | Send data to websocket
sendWs :: WS.Connection -> IO ()
sendWs conn = do
  msg <- mkReq
  sendTextData conn (encode msg)

-- | Receive data from websocket
receiveWs :: Brick.BChan.BChan CustomEvent -> WS.Connection -> IO ThreadId
receiveWs chan conn = do
  forkIO . forever $ do
    bs <- receiveData conn
    
    let msg = decode bs
    forM_ msg $ \case
        MsgError    err -> Brick.BChan.writeBChan chan ErrorMessage
        MsgResponse res -> Brick.BChan.writeBChan chan ResponseMessage
        MsgTicker   t   -> Brick.BChan.writeBChan chan (TickerMessage t)
                        -- print $ fmtTickerRow t

-- | Kill threads
killThreads :: [ThreadId]-> IO ()
killThreads ts = do
  forM_ ts $ (flip throwTo) ThreadKilled

