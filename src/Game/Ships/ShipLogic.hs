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
setMovement ship@Ship{ shipId, position, speed, attachedToBody } bodyId gs@GameState{ orbitTree, time = now } =
  case PlottedPath.plot now position speed bodyId orbitTree of
    Just path | not (elem bodyId attachedToBody) ->
      let ship' = ship
            { movement = Just ShipMovement
              { sourceBodyId = attachedToBody
              , destinationBodyId = bodyId
              , path
              }
            , attachedToBody = Nothing
            }
      in gs & set (#ships . at shipId . _Just) ship'
    _ -> gs

cancelOrder :: Ship -> GameState -> GameState
cancelOrder Ship{ shipId, movement } =
  maybe id (cancelMovement shipId) movement

cancelMovement :: Id Ship -> ShipMovement -> GameState -> GameState
cancelMovement shipId ShipMovement{ sourceBodyId, path } gs =
  gs
    & set (#ships . at shipId . _Just . #movement) Nothing
    & ( if view #startTime path == view #time gs
        then set (#ships . at shipId . _Just . #attachedToBody) sourceBodyId
        else id
      )

update :: GameState -> Ship -> Ship
update gs ship@Ship{ movement } =
  case movement of
    Just ShipMovement{ path, destinationBodyId } ->
      let now = view #time gs
          arrived = now >= view #endTime path
      in ship
        & set #position (PlottedPath.atTime now path)
        & (if arrived then set #movement Nothing else id)
        & set #attachedToBody (if arrived then Just destinationBodyId else Nothing)
    Nothing ->
      case view #attachedToBody ship >>= (\b -> preview (#bodyOrbitalStates . at b . _Just . #position) gs) of
        Just position -> set #position position ship
        Nothing -> ship
