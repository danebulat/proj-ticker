{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Sockets where

import Control.Monad (forever, void, forM_)
import Control.Concurrent (forkIO, ThreadId, threadDelay, throwTo)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically, writeTChan)
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

-- | Initial symbol pairs to subscribe to
initialSymbolPairs :: [Text]
initialSymbolPairs = [ "btcusdt", "ethusdt", "adausdt" ]

-- | Construct and Write ServerRequests to the request channel
writeRequests :: [Text] -> Text -> TChan ServerRequest -> IO ()
writeRequests symbols method chan = forM_ symbols $ \symbol -> do
  randId <- R.randomRIO (1,999)
  req    <- mkReqIO randId method (params symbol)
  atomically $ writeTChan chan req
  where
    params t = [t `T.append` "@miniTicker"]

-- | Connect to Binance WSS in a child thread
connectBinance :: BC.BChan CustomEvent -> TChan ServerRequest -> IO ()
connectBinance eventChan reqChan = do
  -- write initial symbol requests
  writeRequests initialSymbolPairs "SUBSCRIBE" reqChan

  -- spawn thread to handle wss connection
  void . forkIO $
    Wuss.runSecureClient url port path (wsApp eventChan reqChan)
    where
    url  = "stream.binance.com"
    port = 9443
    path = "/ws"

-- | App within web socket connection context
--
-- NOTE: This thread must stay alive until app is closed to
-- preserver the web socket connection.
wsApp :: BC.BChan CustomEvent -> TChan ServerRequest -> WS.ClientApp ()
wsApp eventChan reqChan conn = do

  -- spawn thread to receive responses
  t1 <- receiveWs eventChan conn
  
  -- cache connection and thread ids in app state
  BC.writeBChan eventChan (CacheConnection conn)
  BC.writeBChan eventChan (CacheThreadId t1)

  -- continues in the current thread
  sendWs conn reqChan

-- | Send data to websocket
sendWs :: WS.Connection -> TChan ServerRequest -> IO ()
sendWs conn reqChan =
  forever $ do
    -- read server request from channel
    msg <- atomically $ readTChan reqChan  
    WS.sendTextData conn (encode msg)

-- | Receive data from websocket
receiveWs :: BC.BChan CustomEvent -> WS.Connection -> IO ThreadId
receiveWs chan conn = do
  forkIO . forever $ do
    bs <- WS.receiveData conn
    let msg = decode bs
    forM_ msg $ \case
        MsgError    err -> BC.writeBChan chan (ErrorMessage err)
        MsgResponse res -> BC.writeBChan chan (ResponseMessage res)
        MsgTicker   t   -> BC.writeBChan chan (TickerMessage t)

-- | Kill threads
killThreads :: [ThreadId]-> IO ()
killThreads ts = do
  forM_ ts $ (flip throwTo) ThreadKilled

-- -------------------------------------------------------------------
-- Makers 

mkReq :: ServerRequest
mkReq = ServerRequest 1 "SUBSCRIBE" ["btcusdt@miniTicker"]

mkReqIO :: Int -> Text -> [Text] -> IO ServerRequest
mkReqIO reqId method params = return $ ServerRequest reqId method params

mkReqIO' :: IO ServerRequest
mkReqIO' = do
  rid <- R.randomRIO (1, 999)
  prompt "> Enter params: "
  params <- getLine
  return $ ServerRequest rid "SUBSCRIBE" [T.strip (T.pack params)]
  where
    prompt p = putStrLn p
