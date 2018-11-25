module App.Logic.ShipBuilding where

import App.Prelude

import qualified App.Common.IdMap as IdMap
import qualified App.Dimension.Speed as Speed
import qualified App.Logic.Util as Logic.Util
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship

import App.Common.Id (Id(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import Data.String (fromString)

build :: Id Body -> GameState -> GameState
build bodyId gs@GameState{ colonies, bodyOrbitalStates } =
  fromMaybe gs $ do
    colony@Colony{ shipBuildingTask } <- colonies ^. at bodyId
    task@ShipBuildingTask{ size, buildEffortSpent } <- shipBuildingTask
    let buildEffortNow = dailyBuildEffort colony
        remainingBuildEffort = buildEffortNeeded size - buildEffortSpent - buildEffortNow
    if remainingBuildEffort > 0
    then continueBuilding buildEffortNow
    else finishBuilding task
  where
    continueBuilding buildEffortNow =
      Just
        ( gs & #colonies . at bodyId . _Just . #shipBuildingTask . _Just . #buildEffortSpent
            +~ buildEffortNow
        )

    finishBuilding ShipBuildingTask{ typ, size } = do
      orbitalState <- bodyOrbitalStates ^. at bodyId
      let shipId = gs ^. #ships & IdMap.nextId
          ship = shipBuiltAt bodyId orbitalState shipId typ size
      Just
        ( gs
            & #colonies . at bodyId . _Just . #shipBuildingTask .~ Nothing
            & #ships . at shipId .~ Just ship
        )

startBuilding :: Ship.Type -> Int -> Colony -> GameState -> GameState
startBuilding typ size colony@Colony{ bodyId, shipBuildingTask, stockpile } gs =
  fromMaybe gs $ do
    _ <- shipBuildingTask ^? _Nothing
    let cost = resourcesNeeded size
    stockpileAfterCost <- Logic.Util.payResources stockpile cost
    let task = ShipBuildingTask { typ, size, buildEffortSpent = 0 }
        colony' = colony
          { stockpile = stockpileAfterCost
          , shipBuildingTask = Just task
          }
    Just (gs & #colonies . at bodyId . _Just .~ colony')

cancel :: Id Body -> GameState -> GameState
cancel bodyId gs =
  gs & #colonies . at bodyId . _Just . #shipBuildingTask .~ Nothing
-- TODO give cost back

buildEffortNeeded :: Int -> Int
buildEffortNeeded size = 100 * size

resourcesNeeded :: Int -> HashMap Resource Double
resourcesNeeded size =
  fromIntegral size *^ [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]

dailyBuildEffort :: Colony -> Int
dailyBuildEffort Colony{ installations } =
  let shipyards = installations ^. at Installation.Shipyard . non 0
  in 2 * shipyards

shipBuiltAt :: Id Body -> OrbitalState -> Id Ship -> Ship.Type -> Int -> Ship
shipBuiltAt bodyId OrbitalState{ position } shipId@(Id shipNo) typ size =
  let capability =
        case typ of
          Ship.FreighterType ->
            Ship.Freighter
              Ship.FreighterCapability { cargoCapacity = 1000, loadedCargo = mempty }
          Ship.ColonyShipType ->
            Ship.ColonyShip
              Ship.ColonyShipCapability { cabinCapacity = 1000, loadedPopulation = 0 }
      speed = Speed.kmPerSec (20 * fromIntegral size)
  in Ship
    { Ship.shipId = shipId
    , Ship.name = fromString $ "Ship " ++ show shipNo
    , Ship.size = size
    , Ship.position = position
    , Ship.capability = capability
    , Ship.speed = speed
    , Ship.order = Nothing
    , Ship.attachedToBody = Just bodyId
    }
