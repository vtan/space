module Game.Dimension.Time where

import GlobalImports

import qualified Game.Common.Display as Display

import Game.Common.Display (Display, display)

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

instance Display (Time Int) where
  display (Time t) =
    let
      secs = t `rem` 60
      mins = t `quot` 60 `rem` 60
      hrs = t `quot` 3600 `rem` 24
      day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (daysInMonth * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
    in
      display year <> "-" <> Display.int02 month <> "-" <> Display.int02 day
        <> " " <> Display.int02 hrs <> ":" <> Display.int02 mins <> ":" <> Display.int02 secs

newtype Date a = Date (Time a)

instance Display (Date Int) where
  display (Date (Time t)) =
    let
      day = 1 + t `quot` (24 * 3600) `rem` 30
      month = 1 + t `quot` (daysInMonth * 24 * 3600) `rem` 12
      year = 2100 + t `quot` (12 * 30 * 24 * 3600)
    in display year <> "-" <> Display.int02 month <> "-" <> Display.int02 day

newtype Duration a = Duration (Time a)

instance Display (Duration Int) where
  display (Duration (Time secs))
    | secs < 60 = display secs <> " secs"
    | secs < 3600 = display (fromIntegral secs / 60 :: Double) <> " minutes"
    | secs < 24 * 3600 = display (fromIntegral secs / 3600 :: Double) <> " hours"
    | otherwise = display (fromIntegral secs / 24 / 3600 :: Double) <> " days"
