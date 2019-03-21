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
  , size :: Int
  , capability :: Capability
  , position :: V2 (Local Double)
  , speed :: Speed Double
  , order :: Maybe Order
  , attachedToBody :: Maybe (Id Body)
  }
  deriving (Show, Generic)

data Type
  = FreighterType
  | ColonyShipType
  deriving (Show, Generic, Eq, Enum, Bounded)

types :: [Type]
types = [minBound .. maxBound]

data Capability
  = Freighter FreighterCapability
  | ColonyShip ColonyShipCapability
  deriving (Show, Generic)

data FreighterCapability = FreighterCapability
  { cargoCapacity :: Double
  , loadedCargo :: HashMap Resource Double
  }
  deriving (Show, Generic)

data ColonyShipCapability = ColonyShipCapability
  { cabinCapacity :: Int
  , loadedPopulation :: Int
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
