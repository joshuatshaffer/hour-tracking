module Compute where

import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map

binDays :: [(ZonedTime,ZonedTime,String)] -> Data.Map.Map Day [(TimeOfDay,TimeOfDay,String)]
binDays = alexander . (>>= fredric)

zoneTimes :: [(POSIXTime,POSIXTime,String)] -> IO [(ZonedTime,ZonedTime,String)]
zoneTimes = mapM zoneTimes'
  where
    zoneTimes' (s,f,d) = do s' <- foo s
                            f' <- foo f
                            return (s',f',d)
    foo = utcToLocalZonedTime . posixSecondsToUTCTime

-- The end of the day as s is on.
endOfDay :: ZonedTime -> ZonedTime
endOfDay (ZonedTime t tz) = ZonedTime t' tz
  where t' = LocalTime (localDay t) (TimeOfDay 23 59 59)

-- The begining of the day after s is on.
-- startOfTomorrow s == endOfDay s + ε
startOfTomorrow :: ZonedTime -> ZonedTime
startOfTomorrow (ZonedTime t tz) = ZonedTime t' tz
  where t' = LocalTime (addDays 1 $ localDay t) midnight

-- Does not handle shift spanning a timezone change.
fredric :: (ZonedTime,ZonedTime,String) -> [(Day, (TimeOfDay,TimeOfDay,String))]
fredric (s,f,d)
  | dayOf s == dayOf f = [(dayOf s, (tod s, tod f, d))]
  | otherwise          = (dayOf s, (tod s, firstDayEnd, d)) : fredric (secondDayStart,f,d)
  where
    dayOf = localDay . zonedTimeToLocalTime
    tod = localTimeOfDay . zonedTimeToLocalTime
    firstDayEnd = tod $ endOfDay s
    secondDayStart = startOfTomorrow s

alexander :: [(Day, (TimeOfDay,TimeOfDay,String))] -> Data.Map.Map Day [(TimeOfDay,TimeOfDay,String)]
alexander = foldl alexander' Data.Map.empty
  where alexander' m (d,(s,f,desc)) = Data.Map.insertWith (++) d [(s,f,desc)] m
