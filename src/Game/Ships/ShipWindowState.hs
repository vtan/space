module Game.Ships.ShipWindowState where

import GlobalImports

import Game.Common.Id (Id)
import Game.Ships.Ship (Ship)

data ShipWindowState = ShipWindowState
  { selectedShipId :: Maybe (Id Ship)
  , selectedShipIdScroll :: Double
  }
  deriving (Generic)

initial :: ShipWindowState
initial = ShipWindowState
  { selectedShipId = Nothing
  , selectedShipIdScroll = 0
  }
