module App.Logic.Colony where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask
import qualified Data.HashMap.Strict as HashMap

import App.Common.Uid (Uid(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import Data.String (fromString)

foundColony :: Uid Body -> GameState -> GameState
foundColony bodyUid gs =
  gs & #colonies . at bodyUid .~ Just Colony
    { bodyUid
    , population = 0
    , isHomeworld = False
    , stockpile = mempty
    , installations = mempty
    , buildingTask = Nothing
    , shipBuildingTask = Nothing
    , miningPriorities = zip Resource.minerals (repeat 1) & HashMap.fromList
    }

startBuildingTask :: Uid Body -> Installation -> GameState -> GameState
startBuildingTask bodyUid installation gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    fromMaybe colony $ do
      guard (colony ^. #buildingTask & has _Nothing)
      let cost = buildCost installation
          finishTime = (gs ^. #time) + buildTime installation
          newTask = BuildingTask{ installation, quantity = 1, BuildingTask.finishTime }
      paidColony <- payResourceCost cost colony
      pure $
        paidColony & #buildingTask .~ Just newTask

buildCost :: Installation -> HashMap Resource Double
buildCost = \case
  Installation.Infrastructure ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Mine ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]

buildTime :: Installation -> Time Int
buildTime = \case
  Installation.Infrastructure -> 10 & Time.days
  Installation.Mine -> 20 & Time.days

cancelBuildingTask :: Uid Body -> GameState -> GameState
cancelBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #buildingTask .~ Nothing

startShipBuildingTask :: Uid Body -> GameState -> GameState
startShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just %~ \colony ->
    fromMaybe colony $ do
      guard (colony ^. #shipBuildingTask & has _Nothing)
      let cost = shipCost
          finishTime = (gs ^. #time) + shipBuildTime
          newTask = ShipBuildingTask{ ShipBuildingTask.finishTime }
      paidColony <- payResourceCost cost colony
      pure $
        paidColony & #shipBuildingTask .~ Just newTask

shipCost :: HashMap Resource Double
shipCost =
  [ (Resource.Cadrium, 1000), (Resource.Erchanite, 100), (Resource.Tellerite, 100) ]

shipBuildTime :: Time Int
shipBuildTime = 30 & Time.days

cancelShipBuildingTask :: Uid Body -> GameState -> GameState
cancelShipBuildingTask bodyUid gs =
  gs & #colonies . at bodyUid . _Just . #shipBuildingTask .~ Nothing

installInstallation :: Installation -> Int -> Colony -> GameState -> GameState
installInstallation installation qty colony@Colony{ bodyUid, stockpile } gs =
  fromMaybe gs $ do
    availableMass <- stockpile ^. at (Resource.Installation installation)
    let qtyToInstall = min qty (floor (availableMass / Installation.mass))
        massToInstall = fromIntegral qtyToInstall * Installation.mass
        colony' = colony
          & #installations . at installation . non 0 +~ qtyToInstall
          & #stockpile . at (Resource.Installation installation) . non 0 -~ massToInstall
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

uninstallInstallation :: Installation -> Int -> Colony -> GameState -> GameState
uninstallInstallation installation qty colony@Colony{ bodyUid, installations } gs =
  fromMaybe gs $ do
    installedQty <- installations ^. at installation
    let qtyToUninstall = min qty installedQty
        massToUninstall = fromIntegral qtyToUninstall * Installation.mass
        colony' = colony
          & #stockpile . at (Resource.Installation installation) . non 0 +~ massToUninstall
          & #installations . at installation . non 0 -~ qtyToUninstall
    pure $ gs
      & #colonies . at bodyUid .~ Just colony'

colonyMaxPopulation :: Body -> Colony -> Maybe Int
colonyMaxPopulation Body{ colonyCost } Colony{ isHomeworld, installations } =
  case colonyCost of
    _ | isHomeworld -> Nothing
    Nothing -> Just 0
    Just cc ->
      let installationQty = installations ^. at Installation.Infrastructure . non 0
      in Just $ floor (5000 / cc * fromIntegral installationQty)

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

payResourceCost :: HashMap Resource Double -> Colony -> Maybe Colony
payResourceCost cost colony =
  cost & ifoldlM
    (\resource colonyAcc costQty ->
      let (remainingQty, colony') = colonyAcc & #stockpile . at resource . non 0 <-~ costQty
      in if remainingQty >= 0
        then Just colony'
        else Nothing
    )
    colony
