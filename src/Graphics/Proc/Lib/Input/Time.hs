module Graphics.Proc.Lib.Input.Time(
	year, month, day, utcHour, hour, minute, second, millis	
) where

import Control.Monad.Trans.State.Strict
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

import Graphics.Proc.Core

import Data.Fixed

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

-- | The year() function returns the current year as an integer (2003, 2004, 2005, etc). 
--
-- processing docs: <https://processing.org/reference/year_.html>
year :: Pio Int
year = liftIO $ fmap (\(y, _, _) -> fromInteger y) date

-- | The month() function returns the current month as a value from 1 - 12. 
--
-- processing docs: <https://processing.org/reference/month_.html>
month :: Pio Int
month = liftIO $ fmap (\(_, m, _) -> m) date

-- | The day() function returns the current day as a value from 1 - 31. 
--
-- processing docs: <https://processing.org/reference/day_.html>
day :: Pio Int
day = liftIO $ fmap (\(_, _, d) -> d) date

-- getTime = liftIO $ fmap (\(UTCTime _ time) -> time) getCurrentTime
getTime = liftIO $ do
  fmap (localTimeOfDay  . zonedTimeToLocalTime) getZonedTime  

getUtcTime = liftIO $ fmap utctDayTime getCurrentTime

-- | Returens univeral hour.
utcHour :: Pio Int
utcHour = fmap toHour getUtcTime
  where
    toHour x = (floor x) `div` (60 * 60)  

-- | The hour() function returns the current hour as a value from 0 - 23. 
--
-- processing docs: <https://processing.org/reference/hour_.html>
hour :: Pio Int
hour = fmap todHour getTime

-- |  The minute() function returns the current minute as a value from 0 - 59. 
--
-- processing docs: <https://processing.org/reference/minute_.html>
minute :: Pio Int
minute = fmap todMin getTime

-- | The second() function returns the current second as a value from 0 - 59. 
-- 
-- processing docs: <https://processing.org/reference/second_.html>
second :: Pio Int
second = liftIO $ fmap toSecond $ getCurrentTime >>= return . fromRational . toRational . utctDayTime
  where 
    toSecond x = (floor x) `mod` 60

-- | Returns the number of milliseconds (thousandths of a second) since starting the program. This information is often used for timing events and animation sequences. 
--
--  processing docs: <https://processing.org/reference/millis_.html>
millis :: Pio Int 
millis = do
  start <- getStartTime
  now   <- liftIO $ getCurrentTime
  return $ floor $ (* 1000) $ diffUTCTime now start

