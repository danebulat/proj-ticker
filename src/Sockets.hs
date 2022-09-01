{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Sockets where

import Control.Monad (forever, void, forM_)
import Control.Concurrent (forkIO, ThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException(ThreadKilled))
import Data.Aeson (encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Brick.BChan as BC
import qualified Wuss
import Formatting
import qualified System.Random as R
import WebTypes
import UITypes

-- -------------------------------------------------------------------
-- Web Sockets

-- | Connect to Binance WSS in a child thread
connectBinance :: BC.BChan CustomEvent -> IO ()
connectBinance chan = do
  void . forkIO $ Wuss.runSecureClient url port path (wsApp chan)
    where
    url  = "stream.binance.com"
    port = 9443
    path = "/ws"

-- | App within web socket connection context 
wsApp ::  BC.BChan CustomEvent -> WS.ClientApp ()
wsApp chan conn = do
  --putStrLn "Connected!"
  t1 <- receiveWs chan conn
  sendWs conn

  -- cache connection and thread ids in app state
  BC.writeBChan chan (CacheConnection conn)
  BC.writeBChan chan (CacheThreadId t1)

  -- NOTE: THIS THREAD NEEDS TO STAY OPEN
  -- TODO: Implement channels
  threadDelay (10 ^ 6 * 60 * 60) -- keep alive for an hour

-- | Send data to websocket
sendWs :: WS.Connection -> IO ()
sendWs conn = do
  msg <- mkReq
  WS.sendTextData conn (encode msg)

-- | Receive data from websocket
receiveWs :: BC.BChan CustomEvent -> WS.Connection -> IO ThreadId
receiveWs chan conn = do
  forkIO . forever $ do
    bs <- WS.receiveData conn
    
    let msg = decode bs
    forM_ msg $ \case
        MsgError    err -> BC.writeBChan chan ErrorMessage
        MsgResponse res -> BC.writeBChan chan ResponseMessage
        MsgTicker   t   -> BC.writeBChan chan (TickerMessage t)

-- | Kill threads
killThreads :: [ThreadId]-> IO ()
killThreads ts = do
  forM_ ts $ (flip throwTo) ThreadKilled

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
