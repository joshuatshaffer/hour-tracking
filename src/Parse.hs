
module Parse where

import Data.Time
import Data.Time.Clock.POSIX
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

parceShift :: String -> (POSIXTime,POSIXTime,String)
parceShift s = (r t0, r tf, desc)
  where
    r = fromInteger . read
    (t0,s') = chopOn " " s
    (tf,desc) = chopOn " - " s'

parceShifts :: String -> [(POSIXTime,POSIXTime,String)]
parceShifts = map parceShift . lines'

zoneTimes :: [(POSIXTime,POSIXTime,String)] -> IO [(ZonedTime,ZonedTime,String)]
zoneTimes = mapM zoneTimes'
  where
    zoneTimes' (s,f,d) = do s' <- foo s
                            f' <- foo f
                            return (s',f',d)
    foo = utcToLocalZonedTime . posixSecondsToUTCTime
