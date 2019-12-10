module Game.Ships.ShipLogic where

import GlobalImports

import qualified Game.GameState as GameState
import qualified Game.Dimension.Speed as Speed
import qualified Game.Dimension.Time as Time
import qualified Game.Ships.PlottedPath as PlottedPath

import Game.GameState (GameState(..))
import Game.Common.Id (Id(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitalState (OrbitalState(..))
import Game.Dimension.Speed (Speed(..))
import Game.Ships.Ship (Ship(..), ShipDesign(..), ShipModule(..), ShipMovement(..), ShipOrder(..), ShipOrderType(..))

import Data.String (fromString)

new :: Id Ship -> ShipDesign -> Id Body -> GameState -> GameState
new shipId design initialBodyId gs =
  let
    name = "Ship " <> fromString (show (getInt shipId))
    OrbitalState{ position } = GameState.expectOrbitalState initialBodyId gs
    speed = speedOfDesign design
    movement = Nothing
    order = Nothing
    attachedToBody = Just initialBodyId
    cargoCapacity = cargoCapacityOfDesign design
    loadedCargo = mempty
    ship = Ship{ shipId, name, design, position, speed, movement, order, attachedToBody, cargoCapacity, loadedCargo }
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

makeOrder :: Ship -> Id Body -> ShipOrderType -> GameState -> GameState
makeOrder ship@Ship{ shipId, position, speed, attachedToBody } bodyId orderType gs@GameState{ orbitTree, time = now } =
  case orderType of
    FlyToType -> gs'
    ColonizeType -> setColonizeOrder ship' bodyId gs'
  where
    gs' = set (#ships . at shipId . _Just) ship' gs
    ship' =
      case PlottedPath.plot now position speed bodyId orbitTree of
        Just path | not (elem bodyId attachedToBody) ->
          ship
            { movement = Just ShipMovement
              { sourceBodyId = attachedToBody
              , destinationBodyId = bodyId
              , path
              }
            , attachedToBody = Nothing
            }
        _ -> ship

setColonizeOrder :: Ship -> Id Body -> GameState -> GameState
setColonizeOrder Ship{ shipId, movement } bodyId gs@GameState{ time = now } =
  let
    Body{ colonyCost } = GameState.expectBody bodyId gs
    movementEndTime = fromMaybe now . preview (_Just . #path . #endTime) $ movement
  in
    case colonyCost of
      Nothing -> gs
      Just cost ->
        let
          days = floor (2 ** cost)
          endTime = movementEndTime + Time.days days
        in
          gs & set (#ships . at shipId . _Just . #order) (Just Colonize{ endTime })

cancelOrder :: Ship -> GameState -> GameState
cancelOrder Ship{ shipId, movement } =
  maybe id (cancelMovement shipId) movement
    >>> set (#ships . at shipId . _Just . #order) Nothing

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
