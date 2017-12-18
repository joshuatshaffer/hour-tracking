
module Main where

import Parse
import Compute

import Data.Time
import Data.List (sortOn)
import qualified Data.Map

showShift :: (TimeOfDay,TimeOfDay,String) -> String
showShift (s,f,d) = "    " ++ showT s ++ " to " ++ showT f ++ " - " ++ d
  where showT = formatTime defaultTimeLocale "%l:%M%P"

roundToFourths :: RealFrac a => a -> a
roundToFourths = (/ 4) . fromIntegral . round . (* 4)

showWorkDay :: (Day,[(TimeOfDay,TimeOfDay,String)]) -> String
showWorkDay (a,b) = unlines $ (show a ++ " " ++ tallie ++ " hours") : map showShift (sortOn (\(x,_,_)->x) b)
  where
    tallie = show . roundToFourths . (/(1000000000000 * 60 * 60)) . fromInteger . diffTimeToPicoseconds . sum $ map (\(s,f,_) -> timeOfDayToTime f - timeOfDayToTime s) b

main :: IO ()
main = do
  --z <- getCurrentTimeZone
  shifts <- parceShifts <$> readFile "hours.txt"
  timesheet <- binDays <$> zoneTimes shifts
  mapM_ (putStrLn . showWorkDay) . Data.Map.assocs $ timesheet
  --s <- map (first (let f = utcToLocalTime z . posixSecondsToUTCTime in f *** f)) . parceShifts <$> readFile "hours.txt"
  --mapM_ (putStrLn . showWorkDay) . Data.Map.assocs $ binDays s
