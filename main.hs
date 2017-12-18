#!/usr/bin/env runhaskell

module Main where

import Data.Time
import Data.Time.Clock.POSIX
import Data.List (stripPrefix,sortOn)
import qualified Data.Map
import Data.Char (isSpace)
import Control.Arrow (first,(***))

inReverse :: ([a] -> [b]) -> [a] -> [b]
inReverse f = reverse . f . reverse

trim :: String -> String
trim = inReverse (dropWhile isSpace) . dropWhile isSpace

lines' :: String -> [String]
lines' = filter (not . null) . map trim . lines

chopOn :: Eq a => [a] -> [a] -> ([a],[a])
chopOn _ [] = ([],[])
chopOn d t@(x:xs) = case stripPrefix d t of
                      Just t' -> ([],t')
                      Nothing -> first (x:) $ chopOn d xs

parceShift :: String -> ((POSIXTime,POSIXTime),String)
parceShift s = ((r t0, r tf), desc)
  where
    r = fromInteger . read
    (t0,s') = chopOn " " s
    (tf,desc) = chopOn " - " s'

parceShifts :: String -> [((POSIXTime,POSIXTime),String)]
parceShifts = map parceShift . lines'

binDays :: [((LocalTime,LocalTime),String)] -> Data.Map.Map Day [((TimeOfDay,TimeOfDay),String)]
binDays = foldl (flip binDays') Data.Map.empty
  where
    binDays' ((s,f),d) = if localDay s /= localDay f
                          then binDays' ((s,de),d) . binDays' ((ds,f),d)
                          else Data.Map.insertWith (++) (localDay s) [((localTimeOfDay s, localTimeOfDay f), d)]
                      where
                        de = LocalTime (localDay s) (TimeOfDay 23 59 59)
                        ds = LocalTime (addDays 1 $ localDay s) midnight

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
