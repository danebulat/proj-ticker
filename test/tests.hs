{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec 
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import WebTypes

-- Test encoding a server request 
testEncodeSrq :: Bool
testEncodeSrq = encode srqSample == srqBs

-- test decoding a server request
testDecodeSrq :: Bool
testDecodeSrq = decode srqBs == Just srqSample

-- sample encoded server request
srqBs :: B.ByteString
srqBs = "{\"params\":[\"btcusdt@miniTicker\"],\"method\":\"SUBSCRIBE\",\"id\":1}"

-- sample server request
srqSample :: ServerRequest
srqSample =
  ServerRequest
    { _srqRequestId = 1
    , _srqMethod    = "SUBSCRIBE"
    , _srqParams    = ["btcusdt@miniTicker"]
    }

-- -------------------------------------------------------------------
-- Main 

main :: IO ()
main = hspec $ do

  describe "JSON Encoding" $ do
    it "ServerRequest object encodes correctly" $ do
      testEncodeSrq

  describe "JSON Decoding" $ do
    it "ServerRequest bytestring decodes correctly" $ do
      testDecodeSrq
