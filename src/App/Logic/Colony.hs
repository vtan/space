module App.Logic.Colony where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask

import App.Common.Uid (Uid(..))
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import Data.String (fromString)

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony
    { population = 0
    , isHomeworld = False
    , stockpile = mempty
    , installations = mempty
    , buildingTask = Nothing
    , shipBuildingTask = Nothing
    }

startBuildingTask :: Uid Body -> Installation -> GameState -> GameState
startBuildingTask bodyUid installation gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at Resource.Mineral . non 0
        cost = 1000
        quantity = 500
        finishTime = (gs ^. #time) + 30 * 24 * 3600
        newTask = BuildingTask{ installation, quantity, BuildingTask.finishTime }
    in case colony ^. #buildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at Resource.Mineral . non 0 -~ cost
          & #buildingTask .~ Just newTask
      _ -> colony

cancelBuildingTask :: Uid Body -> GameState -> GameState
cancelBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #buildingTask .~ Nothing

startShipBuildingTask :: Uid Body -> GameState -> GameState
startShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    let buildingMaterials = colony ^. #stockpile . at Resource.Mineral . non 0
        cost = 2000
        finishTime = (gs ^. #time) + 1 * 24 * 3600
        newTask = ShipBuildingTask{ ShipBuildingTask.finishTime }
    in case colony ^. #shipBuildingTask of
      Nothing | buildingMaterials >= cost ->
        colony
          & #stockpile . at Resource.Mineral . non 0 -~ cost
          & #shipBuildingTask .~ Just newTask
      _ -> colony

cancelShipBuildingTask :: Uid Body -> GameState -> GameState
cancelShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing

installInstallation :: Installation -> Int -> Uid Body -> Colony -> GameState -> GameState
installInstallation installation qty bodyUid colony gs =
  fromMaybe gs $ do
    availableQty <- colony ^. #stockpile . at (Resource.Installation installation)
    let installedQty = min qty availableQty
        colony' = colony
          & #installations . at installation . non 0 +~ installedQty
          & #stockpile . at (Resource.Installation installation) . non 0 -~ installedQty
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

uninstallInstallation :: Installation -> Int -> Uid Body -> Colony -> GameState -> GameState
uninstallInstallation installation qty bodyUid colony gs =
  fromMaybe gs $ do
    installedQty <- colony ^. #installations . at installation
    let uninstalledQty = min qty installedQty
        colony' = colony
          & #stockpile . at (Resource.Installation installation) . non 0 +~ uninstalledQty
          & #installations . at installation . non 0 -~ uninstalledQty
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

colonyMaxPopulation :: Body -> Colony -> Maybe Int
colonyMaxPopulation Body{ colonyCost } Colony{ isHomeworld, installations } =
  case colonyCost of
    _ | isHomeworld -> Nothing
    Nothing -> Just 0
    Just cc ->
      let installationQty = installations ^. at Installation.Infrastructure . non 0
      in Just $ floor (10 / cc * fromIntegral installationQty)

shipBuiltAt :: Uid Body -> OrbitalState -> Uid Ship -> Ship
shipBuiltAt bodyUid OrbitalState{ position } shipUid@(Uid shipNo) =
  Ship
    { Ship.uid = shipUid
    , Ship.name = fromString $ "Ship " ++ show shipNo
    , Ship.position = position
    , Ship.cargoCapacity = 1000
    , Ship.loadedCargo = mempty
    , Ship.cabinCapacity = 1000
    , Ship.loadedPopulation = 0
    , Ship.speed = Speed.kmPerSec 100
    , Ship.order = Nothing
    , Ship.attachedToBody = Just bodyUid
    }
