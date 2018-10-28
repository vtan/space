module App.Logic.Colony where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Logic.Util as Logic.Util
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Model.ShipBuildingTask as ShipBuildingTask
import qualified Data.HashMap.Strict as HashMap

import App.Common.Id (Id(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import Data.String (fromString)

foundColony :: Id Body -> GameState -> GameState
foundColony bodyId gs =
  gs & #colonies . at bodyId .~ Just Colony
    { bodyId
    , population = 0
    , isHomeworld = False
    , stockpile = mempty
    , installations = mempty
    , buildQueue = []
    , shipBuildingTask = Nothing
    , miningPriorities = zip Resource.minerals (repeat 1) & HashMap.fromList
    }

startShipBuildingTask :: Id Body -> GameState -> GameState
startShipBuildingTask bodyId gs =
  gs & #colonies . at bodyId . _Just %~ \colony ->
    fromMaybe colony $ do
      guard (colony ^. #shipBuildingTask & has _Nothing)
      let cost = shipCost
          finishTime = (gs ^. #time) + shipBuildTime
          newTask = ShipBuildingTask{ ShipBuildingTask.finishTime }
      paidColony <- Logic.Util.payResources cost colony
      pure $
        paidColony & #shipBuildingTask .~ Just newTask

shipCost :: HashMap Resource Double
shipCost =
  [ (Resource.Cadrium, 1000), (Resource.Erchanite, 100), (Resource.Tellerite, 100) ]

shipBuildTime :: Time Int
shipBuildTime = 30 & Time.days

cancelShipBuildingTask :: Id Body -> GameState -> GameState
cancelShipBuildingTask bodyId gs =
  gs & #colonies . at bodyId . _Just . #shipBuildingTask .~ Nothing

installInstallation :: Installation -> Int -> Colony -> GameState -> GameState
installInstallation installation qty colony@Colony{ bodyId, stockpile } gs =
  fromMaybe gs $ do
    availableMass <- stockpile ^. at (Resource.Installation installation)
    let qtyToInstall = min qty (floor (availableMass / Installation.mass))
        massToInstall = fromIntegral qtyToInstall * Installation.mass
        colony' = colony
          & #installations . at installation . non 0 +~ qtyToInstall
          & #stockpile . at (Resource.Installation installation) . non 0 -~ massToInstall
    pure $ gs
      & #colonies . at bodyId .~ Just colony'

uninstallInstallation :: Installation -> Int -> Colony -> GameState -> GameState
uninstallInstallation installation qty colony@Colony{ bodyId, installations } gs =
  fromMaybe gs $ do
    installedQty <- installations ^. at installation
    let qtyToUninstall = min qty installedQty
        massToUninstall = fromIntegral qtyToUninstall * Installation.mass
        colony' = colony
          & #stockpile . at (Resource.Installation installation) . non 0 +~ massToUninstall
          & #installations . at installation . non 0 -~ qtyToUninstall
    pure $ gs
      & #colonies . at bodyId .~ Just colony'

colonyMaxPopulation :: Body -> Colony -> Maybe Int
colonyMaxPopulation Body{ colonyCost } Colony{ isHomeworld, installations } =
  case colonyCost of
    _ | isHomeworld -> Nothing
    Nothing -> Just 0
    Just cc ->
      let installationQty = installations ^. at Installation.Infrastructure . non 0
      in Just $ floor (5000 / cc * fromIntegral installationQty)

shipBuiltAt :: Id Body -> OrbitalState -> Id Ship -> Ship
shipBuiltAt bodyId OrbitalState{ position } shipId@(Id shipNo) =
  Ship
    { Ship.shipId = shipId
    , Ship.name = fromString $ "Ship " ++ show shipNo
    , Ship.position = position
    , Ship.cargoCapacity = 1000
    , Ship.loadedCargo = mempty
    , Ship.cabinCapacity = 1000
    , Ship.loadedPopulation = 0
    , Ship.speed = Speed.kmPerSec 100
    , Ship.order = Nothing
    , Ship.attachedToBody = Just bodyId
    }
