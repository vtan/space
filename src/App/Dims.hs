module App.Dims where

import App.Prelude

import qualified Linear as Lin

newtype AU a = AU { getAU :: a }
  deriving
  ( Generic, Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, Storable, Functor
  , Lin.Epsilon
  )

_AU :: Iso (AU a) (AU b) a b
_AU = iso getAU AU