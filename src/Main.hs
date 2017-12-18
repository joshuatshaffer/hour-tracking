
module Main where

import Parse
import Compute

import Data.Time
import Data.List (sortOn)
import qualified Data.Map
import Text.Printf

durationOfShift :: (ZonedTime,ZonedTime,String) -> Double
durationOfShift (s,f,_) = realToFrac $ diffUTCTime (zonedTimeToUTC f) (zonedTimeToUTC s) / (60*60)

roundToFourths :: RealFrac a => a -> a
roundToFourths = (/ 4) . fromIntegral . round . (* 4)

showShift :: (ZonedTime,ZonedTime,String) -> String
showShift (s,f,d) = "    " ++ showT s ++ " to " ++ showT f ++ " - " ++ d
  where showT = formatTime defaultTimeLocale "%l:%M%P"

showWorkday :: (Day,[(ZonedTime,ZonedTime,String)]) -> String
showWorkday (a, shifts) = unlines $ header : map showShift shifts'
  where
    shifts' = sortOn (\(x,_,_) -> zonedTimeToUTC x) shifts
    header = printf "%s %5.2f hours (%+6.3f)" (show a) tallie' (tallie - tallie')
    tallie = sum $ map durationOfShift shifts
    tallie' = roundToFourths tallie

showTimesheet :: Data.Map.Map Day [(ZonedTime,ZonedTime,String)] -> String
showTimesheet = Data.Map.foldlWithKey (\s d wd -> s ++ "\n" ++ showWorkday (d,wd)) ""

main :: IO ()
main = do
  shifts <- parceShifts <$> readFile "hours.txt"
  timesheet <- binDays <$> zoneTimes shifts
  putStrLn $ showTimesheet timesheet
