module Game.Ships.ShipLogic where

import GlobalImports

import qualified Game.Ships.PlottedPath as PlottedPath

import Game.GameState (GameState(..))
import Game.Ships.Ship (Ship(..), ShipOrder(..))

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
