module App.Update.Logic where

import App.Prelude

import qualified App.Body as Body
import qualified App.PlottedPath as PlottedPath
import qualified App.Ship as Ship

import App.Body (Body(..))
import App.Dims (AU)
import App.GameState (GameState)
import App.Ship (Ship(..))
import App.Uid (Uid(..))
import Data.String (fromString)

stepTime :: Int -> GameState -> GameState
stepTime dt gs =
  let time = dt + gs ^. #time
  in gs
    & #time .~ time
    & #bodies . traversed %~ Body.atTime time
    & (\gs' -> gs' & #ships . traversed %~ updateShip gs')

updateShip :: GameState -> Ship -> Ship
updateShip gs ship = 
  case ship ^. #order of
    Just Ship.MoveToBody{ Ship.path, Ship.bodyUid } -> 
      let now = gs ^. #time
          arrived = now >= path ^. #endTime
      in ship
        & #position .~ (path & PlottedPath.atTime now)
        & (if arrived then #order .~ Nothing else id)
        & #attachedToBody .~ (if arrived then Just bodyUid else Nothing)
    Nothing -> 
      case ship ^. #attachedToBody >>= (\b -> gs ^. #bodies . at b) of
        Just Body{ Body.position } ->
          ship & #position .~ position
        Nothing -> ship

addShip :: V2 (AU Double) -> GameState -> GameState
addShip pos gs =
  let shipNo = length (gs ^. #ships)
      shipUid = Uid shipNo
      ship = Ship
        { Ship.uid = shipUid
        , Ship.name = fromString $ "Ship " ++ show shipNo
        , Ship.position = pos
        , Ship.speed = 1 / 1495970 -- 100 km/s
        , Ship.order = Nothing
        , Ship.attachedToBody = Nothing
        }
  in gs & #ships . at shipUid .~ Just ship

moveShipToBody :: Ship -> Body -> GameState -> GameState
moveShipToBody Ship{ Ship.uid, Ship.position, speed } body gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed body
      orderMay = Ship.MoveToBody <$> pure (body ^. #uid) <*> pathMay
  in gs & #ships . at uid . _Just . #order .~ orderMay

cancelShipOrder :: Ship -> GameState -> GameState
cancelShipOrder Ship{ Ship.uid } gs =
  gs & #ships . at uid . _Just . #order .~ Nothing