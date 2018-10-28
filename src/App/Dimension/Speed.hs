module App.Dimension.Speed where

import App.Prelude

import qualified App.Common.Print as Print
import qualified App.Dimension.Local as Local
import qualified App.Dimension.Time as Time

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

printKmPerSec :: Speed Double -> TextBuilder
printKmPerSec (Speed auPerSec) =
  Print.float0 (auPerSec * auToKm) <> " km/s"

auToKm :: Num a => a
auToKm = 149597000
