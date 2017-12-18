
module Main where

import Parse
import Compute

import Data.Time
import Data.Time.Clock.POSIX
import Data.List (sortOn)
import qualified Data.Map
import Control.Arrow (first,(***))

showShift :: ((TimeOfDay,TimeOfDay),String) -> String
showShift ((s,f),d) = "    " ++ showT s ++ " to " ++ showT f ++ " - " ++ d
  where showT = formatTime defaultTimeLocale "%l:%M%P"

roundToFourths :: RealFrac a => a -> a
roundToFourths = (/ 4) . fromIntegral . round . (* 4)

showWorkDay :: (Day,[((TimeOfDay,TimeOfDay),String)]) -> String
showWorkDay (a,b) = unlines $ (show a ++ " " ++ tallie ++ " hours") : map showShift (sortOn (fst . fst) b)
  where
    tallie = show . roundToFourths . (/(1000000000000 * 60 * 60)) . fromInteger . diffTimeToPicoseconds . sum $ map (\((s,f),_) -> timeOfDayToTime f - timeOfDayToTime s) b

main :: IO ()
main = do
  z <- getCurrentTimeZone
  s <- map (first (let f = utcToLocalTime z . posixSecondsToUTCTime in f *** f)) . parceShifts <$> readFile "hours.txt"
  mapM_ (putStrLn . showWorkDay) . Data.Map.assocs $ binDays s
