{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UITypes where 

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TChan
import Control.Lens.TH
import Data.Default
import qualified Network.WebSockets as WS
import WebTypes

-- -------------------------------------------------------------------
-- Types

-- named resources 
data Name = VP1 deriving (Ord, Show, Eq)

-- app state
data AppState = AppState
  { _ticker    :: Ticker
  , _conn      :: Maybe WS.Connection
  , _threads   :: [ThreadId]
  , _reqChan   :: Maybe (TChan ServerRequest)
  }

instance Default AppState where
  def = AppState { _ticker    = def
                 , _threads   = []
                 , _conn      = Nothing
                 , _reqChan   = Nothing
                 }

data CustomEvent
  = CacheThreadId ThreadId          -- ^ for closing threads safely when app closes 
  | CacheConnection WS.Connection   -- ^ for closing connection safely when app closes 
  | ErrorMessage
  | ResponseMessage
  | TickerMessage Ticker
  | QuitApp

makeLenses 'AppState


