module App.Dimension.Time where

import App.Prelude

import qualified App.Common.Print as Print

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

printDuration :: Time Int -> TextBuilder
printDuration (Time secs)
  | secs < 60 = Print.int secs <> " secs"
  | secs < 3600 = Print.float2 (fromIntegral secs / 60 :: Double) <> " minutes"
  | secs < 24 * 3600 = Print.float2 (fromIntegral secs / 3600 :: Double) <> " hours"
  | otherwise = Print.float2 (fromIntegral secs / 24 / 3600 :: Double) <> " days"

printDate :: Time Int -> TextBuilder
printDate (Time t) =
  let day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (daysInMonth * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
  in Print.int year <> "-" <> Print.int02 month <> "-" <> Print.int02 day

printDateTime :: Time Int -> TextBuilder
printDateTime (Time t) =
  let secs = t `rem` 60
      mins = t `quot` 60 `rem` 60
      hrs = t `quot` 3600 `rem` 24
      day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (daysInMonth * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
  in Print.int year <> "-" <> Print.int02 month <> "-" <> Print.int02 day
    <> " " <> Print.int02 hrs <> ":" <> Print.int02 mins <> ":" <> Print.int02 secs
