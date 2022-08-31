{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Aeson (encode, decode)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B

import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, sendPing)
import qualified Wuss 
import WebTypes

main :: IO ()
main = Wuss.runSecureClient "echo.websocket.events" 443 "/" ws

ws :: ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void . forkIO . forever $ do
    message <- receiveData connection
    print (message :: Text)

  loop
  sendClose connection (pack "Bye!")
  where
    loop = do
      line <- getLine
      unless (null line) $ do
        sendTextData connection (pack line)
        loop
