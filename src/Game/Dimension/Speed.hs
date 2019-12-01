module Game.Dimension.Speed where

import GlobalImports

import qualified Game.Common.Display as Display
import qualified Game.Dimension.Local as Local
import qualified Game.Dimension.Time as Time

import Game.Common.Display (Display, display)
import Game.Dimension.Local (Local)
import Game.Dimension.Time (Time)

newtype Speed a = Speed { getAuPerSec :: a }
  deriving (Show, Generic, Num, Fractional)

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
