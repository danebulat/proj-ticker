{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (forkIO, ThreadId, throwTo, threadDelay)
import Control.Monad (forever, unless, void, forM_)
import Data.Aeson (encode, decode)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified System.Random as R

import qualified Network.WebSockets as WS
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, sendPing)
import qualified Wuss 
import Control.Exception (AsyncException(ThreadKilled))

import WebTypes
import Format

data BinanceConn = BinanceConn
  { url  :: Text
  , port :: Int
  , path :: Text
  }

-- -------------------------------------------------------------------
-- Web Sockets

-- | App within web socket connection context 
wsApp :: ClientApp ()
wsApp conn = do
  putStrLn "Connected!"

  t1 <- receiveWs conn
  sendWs conn
  
  killThreads conn [t1]
  sendClose conn (pack "Bye!")

-- | Send data to websocket
sendWs :: WS.Connection -> IO ()
sendWs conn = do
  putStrLn "> Subscribe? [y/n]"
  i <- getLine
  
  unless (i /= "y") $ do
    msg <- mkReq
    sendTextData conn (encode msg)
    sendWs conn

-- | Receive data from websocket
receiveWs :: WS.Connection -> IO ThreadId
receiveWs conn = do
  forkIO . forever $ do
    bs <- receiveData conn
    let msg = decode bs
    
    forM_ msg $ \case
        MsgError    err -> print err
        MsgResponse res -> print res
        MsgTicker   t   -> print $ fmtTickerRow t

-- | Kill threads
killThreads :: WS.Connection -> [ThreadId]-> IO ()
killThreads conn ts = do
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


-- -------------------------------------------------------------------
-- Main

main :: IO ()
main =
  Wuss.runSecureClient url port path wsApp
  where
    url  = "stream.binance.com"
    port = 9443
    path = "/ws"
