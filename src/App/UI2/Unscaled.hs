module App.UI2.Unscaled where

import App.Prelude

newtype Unscaled a = Unscaled { getUnscaled :: a }
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Functor)
