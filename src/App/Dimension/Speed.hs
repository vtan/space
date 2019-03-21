module App.Dimension.Speed where

import App.Prelude

import qualified App.Common.Display as Display
import qualified App.Dimension.Local as Local
import qualified App.Dimension.Time as Time

import App.Common.Display (Display, display)
import App.Dimension.Local (Local)
import App.Dimension.Time (Time)

newtype Speed a = Speed { getAuPerSec :: a }
  deriving (Show, Generic)

kmPerSec :: Fractional a => a -> Speed a
kmPerSec = (/ auToKm) >>> Speed

div :: (Fractional a, Integral b) => Local a -> Time b -> Speed a
div distance time =
  Speed (Local.toAU distance / fromIntegral (Time.toSeconds time))

mul :: (Num a, Integral b) => Time b -> Speed a -> Local a
mul time (Speed auPerSec) =
  Local.au (fromIntegral (Time.toSeconds time) * auPerSec)

instance Display (Speed Double) where
  display (Speed auPerSec) =
    Display.fixed 0 (auPerSec * auToKm) <> " km/s"

auToKm :: Num a => a
auToKm = 149597000
