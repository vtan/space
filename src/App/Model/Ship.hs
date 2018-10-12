module App.Model.Ship where

import App.Prelude

import App.Dimension.Local (Local)
import App.Dimension.Speed (Speed)
import App.Model.Body (Body)
import App.Model.PlottedPath (PlottedPath)
import App.Model.Resource (Resource)
import App.Uid (Uid)

data Ship = Ship
  { uid :: Uid Ship
  , name :: Text
  , position :: V2 (Local Double)
  , speed :: Speed Double
  , cargoCapacity :: Int
  , loadedCargo :: HashMap Resource Int
  , cabinCapacity :: Int
  , loadedPopulation :: Int
  , order :: Maybe Order
  , attachedToBody :: Maybe (Uid Body)
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6

data Order
  = MoveToBody
    { bodyUid :: Uid Body
    , path :: PlottedPath
    }
  deriving (Show, Generic)
