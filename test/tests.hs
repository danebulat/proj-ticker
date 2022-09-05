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
testEncodeSrq = encode sServerReq == sServerReqEnc

-- decoding a server request
testDecodeSrq :: Bool
testDecodeSrq = decode sServerReqEnc == Just sServerReq

-- encoding a server response 
testEncodeSrp :: Bool
testEncodeSrp = encode sServerResp == sServerRespEnc

-- decoding a server response 
testDecodeSrp :: Bool
testDecodeSrp = decode sServerRespEnc' == Just sServerResp

-- decoding a server error
testDecodeServerError :: Bool
testDecodeServerError = decode sServerErrorEnc == Just sServerError

-- decoding a ticker 
testDecodeTicker :: Bool
testDecodeTicker = decode sTickerEnc == Just sTicker

-- decoding a server message
testDecodeSrvMsg :: Bool
testDecodeSrvMsg =
  let r1 = MsgResponse (fromJust $ decode sServerRespEnc')   == MsgResponse sServerResp
      r2 = MsgError    (fromJust $ decode sServerErrorEnc) == MsgError sServerError
      r3 = MsgTicker   (fromJust $ decode sTickerEnc) == MsgTicker sTicker
  in r1 && r2 && r3

-- -------------------------------------------------------------------
-- Sample Data

-- encoded server request
sServerReqEnc :: B.ByteString
sServerReqEnc = [r|{"id":1,"method":"SUBSCRIBE","params":["btcusdt@miniTicker"]}|]

-- server request
sServerReq :: ServerRequest
sServerReq = ServerRequest
    { _srqRequestId = 1
    , _srqMethod    = "SUBSCRIBE"
    , _srqParams    = ["btcusdt@miniTicker"]
    }

-- server response
sServerResp :: ServerResponse
sServerResp = ServerResponse { _srpRequestId = 1 , _srpResult = [] }

-- encoded server response
sServerRespEnc, sServerRespEnc' :: B.ByteString
sServerRespEnc  = [r|{"result":[],"id":1}|]
sServerRespEnc' = [r|{"result":null,"id":1}|]

-- server error
sServerError :: ServerError
sServerError = ServerError 2 "invalid request."

-- encoded server error 
sServerErrorEnc :: B.ByteString
sServerErrorEnc = [r|{"code": 2, "msg": "invalid request."}|]

-- encoded ticker (encodes in alphabetical order by key)
sTickerEnc :: B.ByteString
sTickerEnc = [r|{
"E": 123456789,
"c": "0.0025",
"h": "0.0025",
"l": "0.0010",
"o": "0.0010",
"q": "18",
"s": "BTCUSDT",
"v": "10000"}|]

-- ticker sample
sTicker :: Ticker
sTicker = Ticker
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
    
