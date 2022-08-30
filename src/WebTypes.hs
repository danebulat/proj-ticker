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
  { _srqRequestId :: Int        -- 1
  , _srqMethod    :: Text       -- SUBSCRIBE. UNSUBSCRIBE 
  , _srqParams    :: [Text]     -- "btcusdt@miniTicker"
  } deriving (Eq, Show)

-- responses 
data ServerResponse = ServerResponse
  { _srpRequestId :: Int        -- 1
  , _srpResult    :: [Text]     -- null (for subscribe, unsubscribe methods)
  } deriving (Eq, Show)

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
    <$> v .: "id"
    <*> (v .: "result" <|> pure [])

