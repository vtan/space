module App.Logic.Building where

import App.Prelude

import qualified App.Common.Queue as Queue
import qualified App.Dimension.Time as Time
import qualified App.Logic.Util as Logic.Util
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource

import App.Common.Uid (Uid(..))
import App.Dimension.Time (Time)
import App.Model.Body (Body(..))
import App.Model.BuildTask (BuildTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Resource (Resource)

build :: Uid Body -> GameState -> GameState
build bodyUid gs@GameState{ colonies } =
  fromMaybe gs $ do
    colony@Colony{ buildQueue } <- colonies ^. at bodyUid
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
      gs & #colonies . at bodyUid . _Just . #buildQueue . _head . #buildEffortSpent
          +~ buildEffortNow

    finishBuilding task@BuildTask{ installation, quantity, installWhenDone } =
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
enqueue installation colony@Colony{ bodyUid } gs =
  fromMaybe gs $ do
    let cost = resourcesNeeded installation
    colonyWithCostPaid@Colony{ buildQueue } <- Logic.Util.payResources cost colony
    let currentlyBuilt = buildQueue ^? _head . #installation
        colony' =
          if installation `elem` currentlyBuilt
          then colonyWithCostPaid & #buildQueue . _head . #quantity +~ 1
          else
            let task = BuildTask
                  { installation, quantity = 1, buildEffortSpent = 0, installWhenDone = True }
            in colonyWithCostPaid & #buildQueue %~ (task :)
    gs
      & #colonies . at bodyUid . _Just .~ colony'
      & Just

-- Assumes diff is either -1 or 1.
changeQuantityInQueue :: Int -> Int -> Colony -> GameState -> GameState
changeQuantityInQueue queueIndex diff colony@Colony{ bodyUid, buildQueue } gs =
  fromMaybe gs $ do
    (newQueue, BuildTask{ installation }) <-
      buildQueue & Queue.update queueIndex
        ( \task@BuildTask{ quantity } ->
            let newQuantity = quantity + diff
            in if newQuantity > 0
            then Just task{ quantity = newQuantity }
            else Nothing
        )
    let cost = fromIntegral diff *^ resourcesNeeded installation
    colonyWithCostPaid <- Logic.Util.payResources cost colony
    gs
      & #colonies . at bodyUid . _Just .~ colonyWithCostPaid{ buildQueue = newQueue }
      & Just

moveUpInQueue :: Int -> Colony -> GameState -> Maybe GameState
moveUpInQueue queueIndex Colony{ bodyUid, buildQueue } gs = do
  newQueue <- buildQueue & Queue.moveUp queueIndex
  gs
    & #colonies . at bodyUid . _Just . #buildQueue .~ newQueue
    & Just

moveDownInQueue :: Int -> Colony -> GameState -> Maybe GameState
moveDownInQueue queueIndex Colony{ bodyUid, buildQueue } gs = do
  newQueue <- buildQueue & Queue.moveDown queueIndex
  gs
    & #colonies . at bodyUid . _Just . #buildQueue .~ newQueue
    & Just

toggleInstallInQueue :: Int -> Colony -> GameState -> GameState
toggleInstallInQueue queueIndex Colony{ bodyUid } gs =
  gs & #colonies . at bodyUid . _Just . #buildQueue . ix queueIndex . #installWhenDone %~ not