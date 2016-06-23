module Graphics.Proc.Lib.Input.Time(
	year, month, day, hour, minute, second, millis	
) where

import Control.Monad.Trans.State.Strict
import Data.Time.Clock
import Data.Time.Calendar

import Graphics.Proc.Core


date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

year :: Pio Int
year = liftIO $ fmap (\(y, _, _) -> fromInteger y) date

month :: Pio Int
month = liftIO $ fmap (\(_, m, _) -> m) date

day :: Pio Int
day = liftIO $ fmap (\(_, _, d) -> d) date

getTime = liftIO $ fmap (\(UTCTime _ time) -> time) getCurrentTime

hour :: Pio Int
hour = fmap toHour getTime
  where
    toHour x = (floor x) `div` (60 * 60)

minute :: Pio Int
minute = fmap toMinute getTime
  where
    toMinute x = (floor x `div` 60) `mod` 60

second :: Pio Int
second = fmap toSecond getTime
  where 
    toSecond x = (floor x) `mod` 60

millis :: Pio Int 
millis = Pio $ do
  start <- fmap globalStartTime get
  now   <- liftIO $ getCurrentTime
  return $ floor $ (* 1000) $ diffUTCTime now start

