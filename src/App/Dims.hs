module App.Dims where

import App.Prelude

newtype AU a = AU { getAU :: a }
  deriving (Generic, Show, Eq, Ord, Num, Fractional, Functor)

_AU :: Iso (AU a) (AU b) a b
_AU = iso getAU AU