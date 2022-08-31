{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, ThreadId, throwTo)
import Control.Monad (forever, unless, void, forM_)
import Data.Aeson (encode, decode)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B

import qualified Network.WebSockets as WS
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, sendPing)
import qualified Wuss 
import WebTypes
import Control.Exception (AsyncException(ThreadKilled))

main :: IO ()
main = Wuss.runSecureClient "echo.websocket.events" 443 "/" ws

-- | App within web socket connection context 
ws :: ClientApp ()
ws conn = do
  putStrLn "Connected!"

  t1 <- receiveWs conn
  sendWs conn
  
  killThreads conn [t1]
  sendClose conn (pack "Bye!")

-- | Loop for echoing
sendWs :: WS.Connection -> IO ()
sendWs conn = do
  line <- getLine
  unless (null line) $ do
    sendTextData conn (pack line)
    sendWs conn 

-- | Receive data from websocket
receiveWs :: WS.Connection -> IO ThreadId
receiveWs conn = do
  forkIO . forever $ do
    message <- receiveData conn
    print (message :: Text)

-- | Kill threads
killThreads :: WS.Connection -> [ThreadId]-> IO ()
killThreads conn ts = do
  forM_ ts $ (flip throwTo) ThreadKilled
