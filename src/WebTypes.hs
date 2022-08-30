{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebTypes where 

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
-- Server Request 

data ServerRequest = ServerRequest
  { _srqRequestId :: Int        -- 1
  , _srqMethod    :: Text       -- SUBSCRIBE. UNSUBSCRIBE 
  , _srqParams    :: [Text]     -- "btcusdt@miniTicker"
  } deriving (Eq, Show)

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
    
