module Game.Ships.Ship where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Common.Id (Id)
import Game.Dimension.Local (Local)
import Game.Dimension.Speed (Speed)
import Game.Ships.PlottedPath (PlottedPath)

data Ship = Ship
  { shipId :: Id Ship
  , name :: Text
  , position :: V2 (Local Double)
  , speed :: Speed Double
  , order :: Maybe ShipOrder
  , attachedToBody :: Maybe (Id Body)
  }
  deriving (Show, Generic)

data ShipOrder
  = MoveToBody
    { bodyId :: Id Body
    , path :: PlottedPath
    }
  deriving (Show, Generic)
