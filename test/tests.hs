{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Hspec
import Data.Aeson (encode, decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as B
import Text.RawString.QQ
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

-- decoding a server error
testDecodeServerError :: Bool
testDecodeServerError = decode srvErrBs == Just srvErrSample

-- decoding a ticker 
testDecodeTicker :: Bool
testDecodeTicker = decode tickerBs == Just tickerSample

-- decoding a server message
testDecodeSrvMsg :: Bool
testDecodeSrvMsg =
  let r1 = MsgResponse (fromJust $ decode srpBs')   == MsgResponse srpSample
      r2 = MsgError    (fromJust $ decode srvErrBs) == MsgError srvErrSample
      r3 = MsgTicker   (fromJust $ decode tickerBs) == MsgTicker tickerSample
  in r1 && r2 && r3

-- -------------------------------------------------------------------
-- Sample Data

-- encoded server request
srqBs :: B.ByteString
srqBs = [r|{"id":1,"method":"SUBSCRIBE","params":["btcusdt@miniTicker"]}|]

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
srpBs  = [r|{"result":[],"id":1}|]
srpBs' = [r|{"result":null,"id":1}|]

-- server error
srvErrSample :: ServerError
srvErrSample = ServerError 2 "invalid request."

-- encoded server error 
srvErrBs :: B.ByteString
srvErrBs = [r|{"code": 2, "msg": "invalid request."}|]

-- encoded ticker
tickerBs :: B.ByteString
tickerBs = [r|{""s": "BTCUSDT",
"E": 123456789,
"o": "0.0010",
"c": "0.0025",
"h": "0.0025",
"l": "0.0010",
"v": "10000",
"q": "18"}|]

-- ticker
tickerSample :: Ticker
tickerSample = Ticker
  { _tckSymbolPair = "BTCUSDT"
  , _tckTs         = 123456789
  , _tckOpen       = 0.0010
  , _tckClose      = 0.0025
  , _tckHigh       = 0.0025
  , _tckLow        = 0.0010
  , _tckVolume     = 10000
  , _tckTrades     = 18
  }

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
    it "ServerRequest data decodes correctly" $ do
      testDecodeSrq

    it "ServerResponse data decodes correctly" $ do
      testDecodeSrp

    it "Server Error data decodes correctly" $ do
      testDecodeServerError

    it "Ticker data decodes correctly" $ do
      testDecodeTicker
    
    it "ServerMsg data decodes correctly" $ do
      testDecodeTicker
    
