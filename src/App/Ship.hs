module App.Ship where

import App.Prelude

import App.Dims (AU)
import App.PlottedPath (PlottedPath)
import App.Uid (Uid)

data Ship = Ship
  { uid :: Uid Ship
  , name :: Text
  , position :: V2 (AU Double)
  , speed :: AU Double
  , path :: Maybe PlottedPath
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6