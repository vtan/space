module App.Dimension.Time where

import App.Prelude

import Text.Printf (printf)

newtype Time a = Time { toSeconds :: a }
  deriving (Generic, Show, Eq, Ord, Enum, Num, Integral, Real, Functor)

seconds :: a -> Time a
seconds = Time

oneSecond :: Num a => Time a
oneSecond = 1 & seconds

minutes :: Num a => a -> Time a
minutes = seconds >>> fmap (60 *)

hours :: Num a => a -> Time a
hours = minutes >>> fmap (60 *)

days :: Num a => a -> Time a
days = hours >>> fmap (24 *)

oneDay :: Num a => Time a
oneDay = 1 & days

nextMidnight :: Integral a => Time a -> Time a
nextMidnight time = (time `quot` oneDay + 1) * oneDay

daysInMonth :: Num a => a
daysInMonth = 30

printDuration :: Time Int -> String
printDuration (Time secs)
  | secs < 60 = printf "%d secs" secs
  | secs < 3600 = printf "%.2f minutes" (fromIntegral secs / 60 :: Double)
  | secs < 24 * 3600 = printf "%.2f hours" (fromIntegral secs / 3600 :: Double)
  | otherwise = printf "%.2f days" (fromIntegral secs / 24 / 3600 :: Double)

printDate :: Time Int -> String
printDate (Time t) =
  let secs = t `rem` 60
      mins = t `quot` 60 `rem` 60
      hrs = t `quot` 3600 `rem` 24
      day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (daysInMonth * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
  in printf "%d-%02d-%02d %02d:%02d:%02d" year month day hrs mins secs
