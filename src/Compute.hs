module Compute where

import Data.Time
import qualified Data.Map

binDays :: [(ZonedTime,ZonedTime,String)] -> Data.Map.Map Day [(ZonedTime,ZonedTime,String)]
binDays = Data.Map.fromListWith (++) . map (\(x,y)->(x,[y])) . concatMap fredric

-- The end of the day as s is on.
endOfDay :: ZonedTime -> ZonedTime
endOfDay (ZonedTime t tz) = ZonedTime t' tz
  where t' = LocalTime (localDay t) (TimeOfDay 23 59 59)

-- The begining of the day after s is on.
-- startOfTomorrow s == endOfDay s + ε
startOfTomorrow :: ZonedTime -> ZonedTime
startOfTomorrow (ZonedTime t tz) = ZonedTime t' tz
  where t' = LocalTime (addDays 1 $ localDay t) midnight

fredric :: (ZonedTime,ZonedTime,String) -> [(Day, (ZonedTime,ZonedTime,String))]
fredric (s,f,d)
  | dayOf s == dayOf f = [(dayOf s, (s, f, d))]
  | otherwise          = (dayOf s, (s, firstDayEnd, d)) : fredric (secondDayStart, f, d)
  where
    dayOf = localDay . zonedTimeToLocalTime
    firstDayEnd = endOfDay s
    secondDayStart = startOfTomorrow s
