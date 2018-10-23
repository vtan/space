module App.Logic.Building where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Dimension.Time as Time
import qualified App.Model.BuildingTask as BuildingTask
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource

import App.Common.Uid (Uid(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)

build :: Uid Body -> GameState -> GameState
build bodyUid gs@GameState{ colonies } =
  fromMaybe gs $ do
    colony@Colony{ buildQueue } <- colonies ^. at bodyUid
    task@BuildingTask{ installation, buildEffortSpent } <- listToMaybe buildQueue
    let buildEffortNow = dailyBuildEffort colony
        remainingBuildEffort =
          buildEffortNeeded installation - buildEffortSpent - buildEffortNow
    Just $
      if remainingBuildEffort > 0
      then (continueBuilding buildEffortNow)
      else finishBuilding task
  where
    continueBuilding buildEffortNow =
      gs & #colonies . at bodyUid . _Just . #buildQueue . _head . #buildEffortSpent
          +~ buildEffortNow

    finishBuilding task@BuildingTask{ installation, quantity, installWhenDone } =
      gs & #colonies . at bodyUid . _Just %~ (removeOneTask >>> installOrStore)
      where
        removeOneTask =
          if quantity == 1
          then #buildQueue %~ drop 1
          else #buildQueue . _head .~ task { quantity = quantity - 1, buildEffortSpent = 0 }

        installOrStore =
          if installWhenDone
          then #installations . at installation . non 0 +~ 1
          else #stockpile . at (Resource.Installation installation) . non 0 +~ Installation.mass

buildEffortNeeded :: Installation -> Int
buildEffortNeeded = \case
  Installation.Infrastructure -> 100
  Installation.Mine -> 200
  Installation.Factory -> 200

resourcesNeeded :: Installation -> HashMap Resource Double
resourcesNeeded = \case
  Installation.Infrastructure ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Mine ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Factory ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]

dailyBuildEffort :: Colony -> Int
dailyBuildEffort Colony{ installations } =
  let factories = installations ^. at Installation.Factory . non 0
  in 2 * factories
