{-# LANGUAGE OverloadedStrings #-}

module Format where 

import Control.Lens
import Data.Default
import Data.Text (Text, append, pack)
import qualified Formatting as F
import Data.Time.Clock.POSIX
import Data.Time.Clock

import WebTypes

(<^>) :: Text -> Text -> Text
(<^>) = append 

showt :: Show a => a -> Text
showt = pack . show

-- | Format any real number to `p` decimal points
-- Ex: fmtPicoText 4 0.00250012930
fmtPicoText :: Real a => Int -> a -> Text
fmtPicoText p = F.sformat (F.fixed p)

-- | Converts milliseconds to UTC time
-- Ex: fmtMsUTC 1661971736489
fmtMsUTC :: Integer -> UTCTime
fmtMsUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

-- | Ouput a row of ticker information
fmtTickerRow :: Ticker -> Text
fmtTickerRow t = farRight
             <^> col (t ^. tckSymbolPair) "" <^> sep 
             <^> col (fmtPicoText 4 $ t ^. tckOpen)   "O" <^> sep 
             <^> col (fmtPicoText 4 $ t ^. tckClose)  "C" <^> sep 
             <^> col (fmtPicoText 4 $ t ^. tckHigh)   "H" <^> sep 
             <^> col (fmtPicoText 4 $ t ^. tckLow)    "L" <^> sep 
             <^> col (fmtPicoText 2 $ t ^. tckVolume) "V" <^> sep 
             <^> col (fmtPicoText 2 $ t ^. tckTrades) "T" <^> farLeft
  where col val p = shwPrefix p <^> val
        farRight = "| "
        farLeft  = " |"
        sep      = " | "
        shwPrefix p = if p /= "" then p <^> ": " else ""
