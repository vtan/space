module App.Dims where

import App.Prelude

newtype AU a = AU { getAU :: a }
  deriving (Generic, Show, Num, Functor)

_AU :: Iso (AU a) (AU b) a b
_AU = iso getAU AU