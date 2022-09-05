{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UITypes where 

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TChan
import Control.Lens.TH
import Data.Default
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Data.Map (Map)
import qualified System.Clock as C
import WebTypes

-- -------------------------------------------------------------------
-- Types

-- named resources 
data Name = Edit1 | VP1 | AddBtn | RemoveBtn  deriving (Ord, Show, Eq)

-- status line messages
data StatusLineMsg
  = ErrProcessingReq
  | ErrTickerAlreadyAdded
  | ErrValidationFailed
  | ErrSymbolInRemovalBuffer
  | ErrTickerListLength
  | ErrNotInTickerList
  | SuccRequestSent
  | SuccTickerRemoved

-- app state
data AppState = AppState
  { _tickers   :: [Ticker]
  , _conn      :: Maybe WS.Connection
  , _threads   :: [ThreadId]
  , _reqChan   :: Maybe (TChan ServerRequest)

    -- UI
  , _focusRing     :: F.FocusRing Name
  , _edit1         :: E.Editor Text Name
  , _statusText    :: Text
  , _processingReq :: Bool

    -- Remove Buffer 
    -- A removed symbol will be cached in this buffer for
    -- some time. Its used to prevent adding late Ticker data
    -- received from WSS after that ticker symbol has been
    -- removed.
    --
    -- The removed symbol cannot be re-added during the buffer.
    -- This will be communicated in the status line.
  , _removeBuffer  :: [(Text, C.TimeSpec)]
  }

data CustomEvent
  = CacheThreadId ThreadId          -- ^ for closing threads safely when app closes 
  | CacheConnection WS.Connection   -- ^ for closing connection safely when app closes 
  | ErrorMessage ServerError
  | ResponseMessage ServerResponse
  | TickerMessage Ticker
  | QuitApp

makeLenses 'AppState


