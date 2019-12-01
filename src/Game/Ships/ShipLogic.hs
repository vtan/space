module Game.Ships.ShipLogic where

import GlobalImports

import qualified Game.GameState as GameState
import qualified Game.Dimension.Speed as Speed
import qualified Game.Ships.PlottedPath as PlottedPath

import Game.GameState (GameState(..))
import Game.Common.Id (Id(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitalState (OrbitalState(..))
import Game.Dimension.Speed (Speed(..))
import Game.Ships.Ship (Ship(..), ShipDesign(..), ShipModule(..), ShipOrder(..))

import Data.String (fromString)

new :: Id Ship -> ShipDesign -> Id Body -> GameState -> GameState
new shipId design initialBodyId gs =
  let
    name = "Ship " <> fromString (show (getInt shipId))
    OrbitalState{ position } = GameState.expectOrbitalState initialBodyId gs
    speed = speedOfDesign design
    order = Nothing
    attachedToBody = Just initialBodyId
    cargoCapacity = cargoCapacityOfDesign design
    loadedCargo = mempty
    ship = Ship{ shipId, name, design, position, speed, order, attachedToBody, cargoCapacity, loadedCargo }
  in
    gs & set (#ships . at shipId) (Just ship)

speedOfDesign :: ShipDesign -> Speed Double
speedOfDesign ShipDesign{ modules } =
 case length modules of
    0 -> 0
    len -> Speed.kmPerSec 100 / fromIntegral len

cargoCapacityOfDesign :: ShipDesign -> Double
cargoCapacityOfDesign ShipDesign{ modules } =
  50 * fromIntegral (view (at CargoModule . non 0) modules)

update :: GameState -> Ship -> Ship
update gs ship =
  case ship ^. #order of
    Just MoveToBody{ path, bodyId } ->
      let now = gs ^. #time
          arrived = now >= path ^. #endTime
      in ship
        & #position .~ (path & PlottedPath.atTime now)
        & (if arrived then #order .~ Nothing else id)
        & #attachedToBody .~ (if arrived then Just bodyId else Nothing)
    Nothing ->
      case ship ^. #attachedToBody >>= (\b -> gs ^? #bodyOrbitalStates . at b . _Just . #position) of
        Just position ->
          ship & #position .~ position
        Nothing -> ship
