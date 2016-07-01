module Graphics.Proc.Core.State.Elements.Time(
    TimeState(..), initTimeState, TimeInterval, getDuration
) where

import Data.Time.Clock
import Control.Monad.Trans.State.Strict

type TimeInterval = Float

data TimeState = TimeState 
  { timeLast      :: UTCTime
  , timeStart     :: UTCTime }

initTimeState = fmap (\x -> TimeState x x) getCurrentTime   

------------------------------------------

getDuration :: StateT TimeState IO TimeInterval
getDuration = StateT $ \s -> do
    let prevTime = timeLast s
    now <- getCurrentTime
    let dt = fromRational $ toRational $ diffUTCTime now prevTime
    return (dt, s { timeLast = now })
