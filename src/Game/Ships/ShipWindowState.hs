module Game.Ships.ShipWindowState where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Common.Id (Id)
import Game.Ships.Ship (Ship, ShipOrderType)

data ShipWindowState = ShipWindowState
  { selectedShipId :: Maybe (Id Ship)
  , selectedShipIdScroll :: Double
  , selectedBodyId :: Maybe (Id Body)
  , selectedBodyIdScroll :: Double
  , selectedOrderType :: Maybe (ShipOrderType)
  , selectedOrderTypeScroll :: Double
  }
  deriving (Generic)

initial :: ShipWindowState
initial = ShipWindowState
  { selectedShipId = Nothing
  , selectedShipIdScroll = 0
  , selectedBodyId = Nothing
  , selectedBodyIdScroll = 0
  , selectedOrderType = Nothing
  , selectedOrderTypeScroll = 0
  }
