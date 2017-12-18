
module Parse (parceShifts) where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.List (stripPrefix)
import Data.Char (isSpace)
import Control.Arrow (first)

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
