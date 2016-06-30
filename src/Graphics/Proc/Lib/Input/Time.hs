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

year :: Pio Int
year = liftIO $ fmap (\(y, _, _) -> fromInteger y) date

month :: Pio Int
month = liftIO $ fmap (\(_, m, _) -> m) date

day :: Pio Int
day = liftIO $ fmap (\(_, _, d) -> d) date

-- getTime = liftIO $ fmap (\(UTCTime _ time) -> time) getCurrentTime
getTime = liftIO $ do
  fmap (localTimeOfDay  . zonedTimeToLocalTime) getZonedTime  

getUtcTime = liftIO $ fmap utctDayTime getCurrentTime

utcHour :: Pio Int
utcHour = fmap toHour getUtcTime
  where
    toHour x = (floor x) `div` (60 * 60)  

hour :: Pio Int
hour = fmap todHour getTime

minute :: Pio Int
minute = fmap todMin getTime
  -- where
  --   toMinute x = (floor x `div` 60) `mod` 60

second :: Pio Int
second = liftIO $ fmap toSecond $ getCurrentTime >>= return . fromRational . toRational . utctDayTime
  where 
    toSecond x = (floor x) `mod` 60

millis :: Pio Int 
millis = Pio $ do
  start <- fmap globalStartTime get
  now   <- liftIO $ getCurrentTime
  return $ floor $ (* 1000) $ diffUTCTime now start

