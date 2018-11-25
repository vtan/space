module App.Logic.Colony where

import App.Prelude

import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified Data.HashMap.Strict as HashMap

import App.Common.Id (Id(..))
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)

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
