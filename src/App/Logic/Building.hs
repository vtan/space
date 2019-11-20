module App.Logic.Building where

import GlobalImports

import qualified Game.Dimension.Time as Time
import qualified App.Logic.Util as Logic.Util
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource

import Game.Common.Id (Id(..))
import Game.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildTask (BuildTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)

build :: Id Body -> GameState -> GameState
build bodyId gs@GameState{ colonies } =
  fromMaybe gs $ do
    colony@Colony{ buildQueue } <- colonies ^. at bodyId
    task@BuildTask{ installation, buildEffortSpent } <- listToMaybe buildQueue
    let buildEffortNow = dailyBuildEffort colony
        remainingBuildEffort =
          buildEffortNeeded installation - buildEffortSpent - buildEffortNow
    Just $
      if remainingBuildEffort > 0
      then continueBuilding buildEffortNow
      else finishBuilding task
  where
    continueBuilding buildEffortNow =
      gs & #colonies . at bodyId . _Just . #buildQueue . _head . #buildEffortSpent
          +~ buildEffortNow

    finishBuilding task@BuildTask{ installation, quantity, installWhenDone } =
      gs & #colonies . at bodyId . _Just %~ (removeOneTask >>> installOrStore)
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
  Installation.Shipyard -> 300

resourcesNeeded :: Installation -> HashMap Resource Double
resourcesNeeded = \case
  Installation.Infrastructure ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Mine ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Factory ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]
  Installation.Shipyard ->
    [ (Resource.Cadrium, 200), (Resource.Erchanite, 50) ]

dailyBuildEffort :: Colony -> Int
dailyBuildEffort Colony{ installations } =
  let factories = installations ^. at Installation.Factory . non 0
  in 2 * factories

finishTime :: Time Int -> Int -> Installation -> Colony -> Time Int
finishTime now effortSpent installation colony =
  now + Time.days
    ( ceiling
      ( fromIntegral (buildEffortNeeded installation - effortSpent)
      / fromIntegral (dailyBuildEffort colony)
      :: Double
      )
    )

enqueue :: Installation -> Colony -> GameState -> GameState
enqueue installation colony@Colony{ bodyId, stockpile, buildQueue } gs =
  fromMaybe gs $ do
    let cost = resourcesNeeded installation
    stockpileAfterCost <- Logic.Util.payResources stockpile cost
    let colonyAfterCost = colony{ stockpile = stockpileAfterCost }
        currentlyBuilt = buildQueue ^? _head . #installation
        colony' =
          if installation `elem` currentlyBuilt
          then colonyAfterCost & #buildQueue . _head . #quantity +~ 1
          else
            let task = BuildTask
                  { installation, quantity = 1, buildEffortSpent = 0, installWhenDone = True }
            in colonyAfterCost & #buildQueue %~ (task :)
    gs
      & #colonies . at bodyId . _Just .~ colony'
      & Just
