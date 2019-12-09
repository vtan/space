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
import Game.Ships.Ship (Ship(..), ShipDesign(..), ShipModule(..), ShipMovement(..))

import Data.String (fromString)

new :: Id Ship -> ShipDesign -> Id Body -> GameState -> GameState
new shipId design initialBodyId gs =
  let
    name = "Ship " <> fromString (show (getInt shipId))
    OrbitalState{ position } = GameState.expectOrbitalState initialBodyId gs
    speed = speedOfDesign design
    movement = Nothing
    attachedToBody = Just initialBodyId
    cargoCapacity = cargoCapacityOfDesign design
    loadedCargo = mempty
    ship = Ship{ shipId, name, design, position, speed, movement, attachedToBody, cargoCapacity, loadedCargo }
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

setMovement :: Ship -> Id Body -> GameState -> GameState
setMovement ship@Ship{ shipId, position, speed } bodyId gs@GameState{ orbitTree, time = now } =
  case PlottedPath.plot now position speed bodyId orbitTree of
    Just path ->
      let ship' = ship
            { movement = Just ShipMovement{ bodyId, path }
            , attachedToBody = Nothing
            }
      in gs & set (#ships . at shipId . _Just) ship'
    Nothing -> gs

update :: GameState -> Ship -> Ship
update gs ship@Ship{ movement } =
  case movement of
    Just ShipMovement{ path, bodyId } ->
      let now = view #time gs
          arrived = now >= view #endTime path
      in ship
        & set #position (PlottedPath.atTime now path)
        & (if arrived then set #movement Nothing else id)
        & set #attachedToBody (if arrived then Just bodyId else Nothing)
    Nothing ->
      case view #attachedToBody ship >>= (\b -> preview (#bodyOrbitalStates . at b . _Just . #position) gs) of
        Just position -> set #position position ship
        Nothing -> ship
