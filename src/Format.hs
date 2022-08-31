module Format where 

import Data.Text (Text)
import qualified Formatting as F
import Data.Time.Clock.POSIX
import Data.Time.Clock

-- | Format any real number to `p` decimal points
-- Ex: fmtPicoText 4 0.00250012930
fmtPicoText :: Real a => Int -> a -> Text
fmtPicoText p = F.sformat (F.fixed p)

-- | Converts milliseconds to UTC time
-- Ex: fmtMsUTC 1661971736489
fmtMsUTC :: Integer -> UTCTime
fmtMsUTC t = posixSecondsToUTCTime $ fromInteger t / 1000
