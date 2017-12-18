module Compute where

import Data.Time
import qualified Data.Map

binDays :: [((LocalTime,LocalTime),String)] -> Data.Map.Map Day [((TimeOfDay,TimeOfDay),String)]
binDays = foldl (flip binDays') Data.Map.empty
  where
    binDays' ((s,f),d) = if localDay s /= localDay f
                          then binDays' ((s,de),d) . binDays' ((ds,f),d)
                          else Data.Map.insertWith (++) (localDay s) [((localTimeOfDay s, localTimeOfDay f), d)]
                      where
                        de = LocalTime (localDay s) (TimeOfDay 23 59 59)
                        ds = LocalTime (addDays 1 $ localDay s) midnight
