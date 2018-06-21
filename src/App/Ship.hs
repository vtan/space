module App.Ship where

import App.Prelude

import App.Dims (AU)
import App.Uid (Uid)

data Ship = Ship
  { uid :: Uid Ship
  , name :: Text
  , position :: V2 (AU Double)
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6