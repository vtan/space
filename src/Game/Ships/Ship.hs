module Game.Ships.Ship where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Bodies.Resource (Resource)
import Game.Common.Display (Display(..))
import Game.Common.Id (Id)
import Game.Dimension.Local (Local)
import Game.Dimension.Speed (Speed)
import Game.Dimension.Time (Time)
import Game.Ships.PlottedPath (PlottedPath)

data Ship = Ship
  { shipId :: Id Ship
  , name :: Text
  , design :: ShipDesign
  , position :: V2 (Local Double)
  , speed :: Speed Double
  , movement :: Maybe ShipMovement
  , order :: Maybe ShipOrder
  , attachedToBody :: Maybe (Id Body)
  , cargoCapacity :: Double
  , loadedCargo :: HashMap Resource Double
  }
  deriving (Show, Generic)

data ShipMovement = ShipMovement
  { sourceBodyId :: Maybe (Id Body)
  , destinationBodyId :: Id Body
  , path :: PlottedPath
  }
  deriving (Show, Generic)

data ShipOrderType
  = FlyToType
  | ColonizeType
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Display ShipOrderType where
  display = \case
    FlyToType -> "Fly to"
    ColonizeType -> "Colonize"

data ShipOrder
  = Colonize { endTime :: Time Int }
  deriving (Show, Generic)

data ShipDesign = ShipDesign
  { name :: Text
  , modules :: HashMap ShipModule Int
  }
  deriving (Show, Generic)

designs :: [ShipDesign]
designs =
  [ ShipDesign
      { name = "Cargo ship"
      , modules = [(CargoModule, 1)]
      }
  , ShipDesign
      { name = "Colony ship"
      , modules = [(ColonyModule, 1)]
      }
  ]

data ShipModule
  = CargoModule
  | ColonyModule
  deriving (Show, Generic, Eq)

instance Hashable ShipModule

instance Display ShipModule where
  display = \case
    CargoModule -> "Cargo module"
    ColonyModule -> "Colony module"
