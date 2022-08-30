{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec 
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import WebTypes

-- -------------------------------------------------------------------
-- Test Functions

-- encoding a server request 
testEncodeSrq :: Bool
testEncodeSrq = encode srqSample == srqBs

-- decoding a server request
testDecodeSrq :: Bool
testDecodeSrq = decode srqBs == Just srqSample

-- encoding a server response 
testEncodeSrp :: Bool
testEncodeSrp = encode srpSample == srpBs

-- decoding a server response 
testDecodeSrp :: Bool
testDecodeSrp = decode srpBs' == Just srpSample


-- -------------------------------------------------------------------
-- Sample Data

-- encoded server request
srqBs :: B.ByteString
srqBs = "{\"params\":[\"btcusdt@miniTicker\"],\"method\":\"SUBSCRIBE\",\"id\":1}"

-- server request
srqSample :: ServerRequest
srqSample = ServerRequest
    { _srqRequestId = 1
    , _srqMethod    = "SUBSCRIBE"
    , _srqParams    = ["btcusdt@miniTicker"]
    }

-- server response
srpSample :: ServerResponse
srpSample = ServerResponse { _srpRequestId = 1 , _srpResult = [] }

-- encoded server response
srpBs, srpBs' :: B.ByteString 
srpBs  = "{\"result\":[],\"id\":1}"
srpBs' = "{\"result\":null,\"id\":1}"


-- -------------------------------------------------------------------
-- Main 

main :: IO ()
main = hspec $ do

  describe "JSON Encoding" $ do
    it "ServerRequest object encodes correctly" $ do
      testEncodeSrq

    it "ServerResponse object encodes correctly" $ do
      testEncodeSrq

  describe "JSON Decoding" $ do
    it "ServerRequest bytestring decodes correctly" $ do
      testDecodeSrq

    it "ServerResponse object decodes correctly" $ do
      testDecodeSrp
