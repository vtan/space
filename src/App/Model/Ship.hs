module App.Model.Ship where

import App.Prelude

import App.Common.Id (Id)
import App.Dimension.Local (Local)
import App.Dimension.Speed (Speed)
import App.Model.Body (Body)
import App.Model.PlottedPath (PlottedPath)
import App.Model.Resource (Resource)

data Ship = Ship
  { shipId :: Id Ship
  , name :: Text
  , position :: V2 (Local Double)
  , speed :: Speed Double
  , cargoCapacity :: Double
  , loadedCargo :: HashMap Resource Double
  , cabinCapacity :: Int
  , loadedPopulation :: Int
  , order :: Maybe Order
  , attachedToBody :: Maybe (Id Body)
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6

data Order
  = MoveToBody
    { bodyId :: Id Body
    , path :: PlottedPath
    }
  deriving (Show, Generic)
