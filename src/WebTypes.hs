{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebTypes where 
import Control.Applicative ((<|>))
import Control.Lens.TH 
import Data.Aeson
import Data.Default
import Data.Fixed (Pico)
import Data.Foldable (asum)
import Data.Maybe
import Data.Map (Map)
import Data.Text (Text, pack)
import GHC.Generics

type FixedFloat = Pico 

-- -------------------------------------------------------------------
-- Models

-- request 
data ServerRequest = ServerRequest
  { _srqRequestId :: Int            -- 1
  , _srqMethod    :: Text           -- SUBSCRIBE. UNSUBSCRIBE 
  , _srqParams    :: [Text]         -- "btcusdt@miniTicker"
  } deriving (Eq, Show)

-- responses 
data ServerResponse = ServerResponse
  { _srpRequestId :: Int            -- 1
  , _srpResult    :: [Text]         -- null (for subscribe, unsubscribe methods)
  } deriving (Eq, Show)

data ServerError = ServerError
  { _sveCode      :: Int            -- 2
  , _sveMessage   :: Text           -- "invalid..."
  } deriving (Eq, Show)

data Ticker = Ticker
  { _tckSymbolPair :: Text          -- "s", symbol,     "BTCUSDF"
  , _tckTs         :: Int           -- "E", event time,  123456789
  , _tckOpen       :: FixedFloat    -- "o", open price,  0.0010
  , _tckClose      :: FixedFloat    -- "c", close price, 0.0025
  , _tckHigh       :: FixedFloat    -- "h", high price,  0.0025
  , _tckLow        :: FixedFloat    -- "l", low price,   0.0010
  , _tckVolume     :: FixedFloat    -- "v", total traded base asset volume, 10000
  , _tckTrades     :: FixedFloat    -- "q", total traded quote asset volume, 10
  } deriving (Eq, Show)

data ServerMsg
  = MsgResponse ServerResponse
  | MsgError    ServerError
  | MsgTicker   Ticker
  deriving (Eq, Show)


-- -------------------------------------------------------------------
-- JSON Encoding and Decoding

-- server request
instance ToJSON ServerRequest where
  toJSON (ServerRequest reqId method params) = object
    [ "id"     .= reqId
    , "method" .= method
    , "params" .= params
    ]

instance FromJSON ServerRequest where
  parseJSON = withObject "ServerRequest" $ \v -> ServerRequest
    <$> v .: "id"
    <*> v .: "method"
    <*> v .: "params"

-- server response
instance ToJSON ServerResponse where
  toJSON (ServerResponse resId result) = object
    [ "id"     .= resId
    , "result" .= result
    ]

instance FromJSON ServerResponse where
  parseJSON = withObject "ServerResonse" $ \v -> ServerResponse
    <$>  v .: "id"
    <*> (v .: "result" <|> pure [])

-- server error
instance ToJSON ServerError where
  toJSON (ServerError code msg) = object
    [ "code" .= code
    , "msg"  .= msg
    ]

instance FromJSON ServerError where
  parseJSON = withObject "ServerError" $ \v -> ServerError
    <$> v .: "code"
    <*> v .: "msg"

-- ticker
instance FromJSON Ticker where
  parseJSON = withObject "Ticker" $ \o -> Ticker
    <$> o .: "s" <*> o .: "E" <*> o .: "o"
                 <*> o .: "c" <*> o .: "h"
                 <*> o .: "l" <*> o .: "v"
                 <*> o .: "q"

-- server message
instance FromJSON ServerMsg where
  parseJSON v = asum
    [ MsgResponse <$> parseJSON v
    , MsgError    <$> parseJSON v
    , MsgTicker   <$> parseJSON v
    ]
