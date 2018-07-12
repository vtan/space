module App.Update.Logic where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship
import qualified Data.HashMap.Strict as HashMap

import App.Model.Body (Body(..))
import App.Model.BodyMinerals (BodyMinerals, MineralData(..))
import App.Model.Colony (Colony(..))
import App.Model.Dims (AU)
import App.Model.GameState (GameState(..))
import App.Model.Ship (Ship(..))
import App.Uid (Uid(..))
import App.Util (reduce)
import Data.String (fromString)

stepTime :: Int -> GameState -> GameState
stepTime dt gs =
  let now = gs ^. #time
      untilTick = timeUntilNextMidnight now
  in if
    | dt == 0 -> gs
    | dt < untilTick -> gs & jumpTimeTo (now + dt)
    | otherwise -> gs
        & jumpTimeTo (now + untilTick)
        & productionTick
        & stepTime (dt - untilTick)

jumpTimeTo :: Int -> GameState -> GameState
jumpTimeTo time gs =
  gs
    & #time .~ time
    & #bodies . traversed %~ Body.atTime time
    & (\gs' -> gs' & #ships . traversed %~ updateShip gs')

timeUntilNextMidnight :: Int -> Int
timeUntilNextMidnight now = (quot now 86400 + 1) * 86400 - now

productionTick :: GameState -> GameState
productionTick gs@GameState{ bodies } =
  gs & reduce bodies (\gs' Body{ Body.uid } ->
      let colony = gs' ^. #colonies . at uid
          minerals = gs' ^. #bodyMinerals . at uid
      in (mineColonies uid <$> colony <*> minerals <*> pure gs')
        & fromMaybe gs'
    )

mineColonies :: Uid Body -> Colony -> BodyMinerals -> GameState -> GameState
mineColonies bodyUid Colony{ mines } minerals gs =
  let mineralsReservesMines = itoList $ HashMap.intersectionWith (,) minerals mines
  in gs & reduce mineralsReservesMines (\gs' (mineral, (MineralData{ available, accessibility }, mineQty)) ->
      let minedQty = min (fromIntegral mineQty * 0.01 * accessibility) available
      in gs'
        & #bodyMinerals . at bodyUid . _Just . at mineral . _Just . #available -~ minedQty
        & #colonies . at bodyUid . _Just . #stockpile . at mineral . non 0 +~ minedQty
    )

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

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony{ stockpile = mempty, mines = mempty }