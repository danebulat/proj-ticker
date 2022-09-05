{-# LANGUAGE OverloadedStrings #-}

module Utils where 

import Control.Lens 
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import System.Clock (TimeSpec, toNanoSecs)
import WebTypes
import UITypes

-- -------------------------------------------------------------------
-- Functions called in event handlers

-- | Takes a symbol and checks if it is cached in the removal buffer.
isInRemoveBuffer :: Text -> [(Text, TimeSpec)] -> Bool
isInRemoveBuffer sym = foldr (\x acc ->
  if acc then acc else T.toUpper sym == fst x) False

-- | Deletes remove buffer items that have been cached for over 10 seconds
updateRemoveBuffer :: TimeSpec -> [(Text, TimeSpec)] -> [(Text, TimeSpec)]
updateRemoveBuffer curTime = filter (go curTime)
  where
    go ct x = let start   = snd x
                  nSecs   = toNanoSecs (ct - start)
                  seconds = fromIntegral nSecs / (10^9)
              in seconds < duration
    duration = 10.0  -- remove symbol from buffer after 10 seconds

-- | Sort tickers alphabetically based on the Ticker symbol pair
sortTickerAlpha :: [Ticker] -> [Ticker]
sortTickerAlpha = sortBy (\t1 t2 ->
  compare (t2^.tckSymbolPair) (t1^.tckSymbolPair))

removeTickerWithSymbol :: Text -> [Ticker] -> [Ticker]
removeTickerWithSymbol x = filter (\t -> (t^.tckSymbolPair) /= symUpper)
  where symUpper = T.toUpper x

-- | Validates editor text when adding a symbol pair
validateSymbol :: Text -> Bool
validateSymbol ts = isAlpha && noSpaces && goodLength
  where validChars = ['A'..'Z'] ++ ['a'..'z']
        isAlpha = foldr (\x acc ->
          if not acc then acc else x `elem` validChars) True (T.unpack ts)
        noSpaces = ' ' `notElem` T.unpack ts
        goodLength = T.length ts > 4

-- | Takes a symbol pair and returns True if corresponding Ticker exists
-- in the provided Ticker list.
searchTickerSymbol :: Text -> [Ticker] -> Bool
searchTickerSymbol t =
  foldr (\x acc -> if acc then acc else T.toUpper t == x ^. tckSymbolPair) False

-- | Takes a Ticker and returns True if a corresponding Ticker with the
-- same symbol pair exists in the provided Ticker list.
searchTicker :: Ticker -> [Ticker] -> Bool
searchTicker t = foldr (\x acc ->
    if acc then acc else x ^. tckSymbolPair == target) False
  where target = t ^. tckSymbolPair

-- | Updates a Ticker list by inserting the provided Ticker, or replacing
-- an older Ticker with the provided Ticker (with the same symbol pair).
rebuildTickers :: Ticker -> [Ticker] -> [Ticker]
rebuildTickers t ts
  | null ts = [t]
  | searchTicker t ts = map (\x -> if x ^. tckSymbolPair == target then t else x) ts
  | otherwise = t : ts
  where
    target = t ^. tckSymbolPair

-- | Status line text messages.
mkStatusText :: StatusLineMsg -> Text
mkStatusText ErrProcessingReq         = "A request is already being handled."
mkStatusText ErrTickerAlreadyAdded    = "The typed symbol is already added."
mkStatusText ErrValidationFailed      = "Validation failed. Enter a valid symbol pair."
mkStatusText ErrSymbolInRemovalBuffer = "Wait a few more seconds to add this symbol again."
mkStatusText ErrTickerListLength      = "At least one ticker must be present."
mkStatusText ErrNotInTickerList       = "The typed symbol pair is not in the ticker list."
mkStatusText SuccRequestSent          = "Request sent for "
mkStatusText SuccTickerRemoved        = " has been removed."

-- | Checks for some potential conditions that prevent removing
-- a ticker.
validateRemoveRequest :: Bool
                      -> [Ticker]
                      -> Text
                      -> (Bool, Maybe StatusLineMsg)
validateRemoveRequest processingReq ts sym
  | processingReq                   = (False, Just ErrProcessingReq)
  | length ts < 2                   = (False, Just ErrTickerListLength)
  | not (searchTickerSymbol sym ts) = (False, Just ErrNotInTickerList)
  | otherwise                       = (True, Nothing)

-- | Checks for some potential conditions that prevent adding
-- a ticker.
validateAddRequest :: Bool
                   -> [Ticker]
                   -> Text
                   -> [(Text, TimeSpec)]
                   -> (Bool, Maybe StatusLineMsg)
validateAddRequest processingReq ts sym rb
  | processingReq             = (False, Just ErrProcessingReq)
  | searchTickerSymbol sym ts = (False, Just ErrTickerAlreadyAdded)
  | not (validateSymbol sym)  = (False, Just ErrValidationFailed)
  | isInRemoveBuffer
    ( T.toUpper
    . T.strip $ sym) rb       = (False, Just ErrSymbolInRemovalBuffer)
  | otherwise                 = (True, Nothing)
