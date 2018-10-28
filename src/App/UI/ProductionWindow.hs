module App.UI.ProductionWindow
  ( update )
where

import App.Prelude

import qualified App.Common.IdMap as IdMap
import qualified App.Dimension.Time as Time
import qualified App.Logic.Building as Logic.Building
import qualified App.Logic.Mining as Logic.Mining
import qualified App.Model.BuildTask as BuildTask
import qualified App.Model.Mineral as Mineral
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified App.Update.WidgetTree as WidgetTree

import App.Common.Util (whenAlt)
import App.Dimension.Time (Time)
import App.Model.BuildTask (BuildTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.Resource (Resource)
import App.Update.Updating (Updating)
import Control.Monad.Reader.Class (local)
import Control.Monad.Zip (munzip)
import Data.String (fromString)

data Action
  = ChangeMiningPriority Resource Int
  | EnqueueBuildingTask Installation
  | ChangeBuildingQueueQuantity Int Int
  | MoveUpInBuildingQueue Int
  | MoveDownInBuildingQueue Int
  | ToggleInstallInBuildingQueue Int

update :: GameState -> Updating GameState
update gs@GameState{ bodies, colonies, bodyMinerals, time } = do
  Updating.useWidget "productionWindow" $ do
    Updating.widget "decoration" $
      Widget.window 20 "Production"

    let bodiesWithColony = toList (IdMap.zip bodies colonies)
    (selectedColony, _) <-
      Updating.widget "selectedBody"
        ( Widget.listBox
            20 (view (_1 . #bodyId)) (view (_1 . #name))
            #selectedBody
            bodiesWithColony
        ) <&> (_1 . mapped %~ snd)

    case selectedColony of
      Just colony@Colony{ bodyId, installations } -> do
        let minerals = bodyMinerals ^. at bodyId . non mempty
        Updating.useWidget "rightPanel" $ do
          Updating.useWidget "installations" $ do
            let mines = installations ^. at Installation.Mine . non 0
            Updating.widget "mineCountLabel" $ Widget.label "Mines"
            Updating.widget "mineCount" $
              Widget.label (fromString (show mines))

            let factories = installations ^. at Installation.Factory . non 0
            Updating.widget "factoryCountLabel" $ Widget.label "Factories"
            Updating.widget "factoryCount" $
              Widget.label (fromString (show factories))

          miningAction <- Updating.useWidget "mining" $
            miningPanel colony minerals
          buildingAction <- Updating.useWidget "building" $
            buildingPanel time colony

          case miningAction <|> buildingAction of
            Just (ChangeMiningPriority resource diff) ->
              pure $ Logic.Mining.changeMiningPriority resource diff colony gs

            Just (EnqueueBuildingTask installation) ->
              pure $ Logic.Building.enqueue installation colony gs

            Just (ChangeBuildingQueueQuantity i diff) ->
              pure $ Logic.Building.changeQuantityInQueue i diff colony gs

            Just (MoveUpInBuildingQueue i) ->
              case Logic.Building.moveUpInQueue i colony gs of
                Just gs' -> gs' <$ (#ui . #selectedBuildingTaskIndex . #selectedIndex . _Just -= 1)
                Nothing -> pure gs

            Just (MoveDownInBuildingQueue i) ->
              case Logic.Building.moveDownInQueue i colony gs of
                Just gs' -> gs' <$ (#ui . #selectedBuildingTaskIndex . #selectedIndex . _Just += 1)
                Nothing -> pure gs

            Just (ToggleInstallInBuildingQueue i) ->
              pure $ Logic.Building.toggleInstallInQueue i colony gs

            Nothing -> pure gs

      Nothing -> pure gs

miningPanel :: Colony -> HashMap Resource Mineral -> Updating (Maybe Action)
miningPanel colony@Colony{ miningPriorities, stockpile } minerals = do
  Updating.useWidget "headingRow" $ do
    let totalMonthlyMined = Time.daysInMonth * Logic.Mining.dailyMinedAtFullAccessibility colony
    Updating.widget "title" $ Widget.label "Mining"
    Updating.widget "totalMined" $
      Widget.label (fromString $ printf "Mined / mo at 100%% acc.: %.0f t" totalMonthlyMined)
    Updating.thisWidget Widget.bottomLine

  Updating.useWidget "headerRow" $ do
    Updating.widget "monthlyMined" $ Widget.label "Mined / mo"
    Updating.widget "priority" $ Widget.label "Priority"
    Updating.widget "mineable" $ Widget.label "Mineable"
    Updating.widget "accessibility" $ Widget.label "Accessibility"
    Updating.widget "stockpile" $ Widget.label "Stockpile"

  Updating.useWidget "mineralRows" $ do
    let allMonthlyMined = Time.daysInMonth *^ Logic.Mining.dailyMined colony minerals
    actions <- ifor Resource.minerals $ \i resource -> do
      let Mineral{ available, accessibility } = minerals ^. at resource . Mineral.nonEmpty
          monthlyMined = allMonthlyMined ^. at resource . non 0
          priority = miningPriorities ^. at resource . non 0
          inStockpile = stockpile ^. at resource . non 0
      Updating.useWidget "row" $ do
        rowHeight <- view (#widgetTree . #bounds . #wh . _y)
        let offsetRow bounds = bounds & #xy . _y +~ i * rowHeight
        local (#widgetTree %~ WidgetTree.mapTree (#bounds %~ offsetRow)) $ do
          Updating.widget "name" $
            Widget.label (fromString $ show resource)
          Updating.widget "monthlyMined" $
            Widget.label (fromString $ printf "%.0f t" monthlyMined)
          Updating.widget "mineable" $
            Widget.label (fromString $ printf "%.0f t" available)
          Updating.widget "accessibility" $
            Widget.label (fromString $ printf "%.0f%%" (100 * accessibility))
          Updating.widget "stockpile" $
            Widget.label (fromString $ printf "%.0f t" inStockpile)
          Updating.widget "priority" $
            Widget.label (fromString $ show priority)

          increasePriority <- Updating.widget "increasePriority"
            (Widget.button "+")
            <&> whenAlt (ChangeMiningPriority resource 1)

          decreasePriority <- Updating.widget "decreasePriority"
            (Widget.button "−")
            <&> whenAlt (ChangeMiningPriority resource (-1))

          pure (increasePriority <|> decreasePriority)

    pure (asum actions)

buildingPanel :: Time Int -> Colony -> Updating (Maybe Action)
buildingPanel now colony@Colony{ buildQueue } = do
  Updating.useWidget "headingRow" $ do
    let monthlyBuildEffort = Time.daysInMonth * Logic.Building.dailyBuildEffort colony
    Updating.widget "title" $ Widget.label "Building"
    Updating.widget "monthlyBuildEffort" $
      Widget.label (fromString $ printf "Build effort / mo: %d" monthlyBuildEffort)
    Updating.thisWidget Widget.bottomLine

  selectedInstallation <- Updating.widget "selectedInstallation" $
    Widget.closedDropdown
      20 380
      id (show >>> fromString)
      #selectedInstallation
      Installation.all

  enqueue <- Updating.widget "enqueue"
    (Widget.button "Enqueue")
    <&> whenAlt (EnqueueBuildingTask <$> selectedInstallation)
    <&> join

  reset <- Updating.widget "reset" $
    Widget.button "Reset"
  when reset (#ui . #selectedInstallation . #selectedIndex .= Nothing)

  for_ selectedInstallation $ \installation -> do
    let buildEffort = Logic.Building.buildEffortNeeded installation
        done = Logic.Building.finishTime now 0 installation colony
        cost = Logic.Building.resourcesNeeded installation
    Updating.widget "newBuildEffort" $
      Widget.label (fromString $ "Build effort: " ++ show buildEffort)
    Updating.widget "newDone" $
      Widget.label (fromString $ "Done: " ++ Time.printDate done)
    Updating.widget "newCost" $
      Widget.label ("Cost: " <> Resource.printCost cost)

  (selectedTaskWithIndex, _) <- Updating.widget "buildQueue" $
    Widget.listBox
      20 (view _1) (view (_2 . to BuildTask.print))
      #selectedBuildingTaskIndex
      (zip [0..] buildQueue)
  let (selectedTaskIndex, selectedTask) = munzip selectedTaskWithIndex

  for_ selectedTask $ \BuildTask{ installation, buildEffortSpent } -> do
    let buildEffort = Logic.Building.buildEffortNeeded installation
        done = Logic.Building.finishTime now buildEffortSpent installation colony
        cost = Logic.Building.resourcesNeeded installation
        doneLabel = if 0 `elem` selectedTaskIndex then "Done: " else "Done if first: "
    Updating.widget "enqueuedBuildEffort" $
      Widget.label (fromString $ printf "Build effort: %d / %d" buildEffortSpent buildEffort)
    Updating.widget "enqueuedDone" $
      Widget.label (fromString $ doneLabel ++ Time.printDate done)
    Updating.widget "enqueuedCost" $
      Widget.label ("Cost: " <> Resource.printCost cost)

  increaseQuantity <- Updating.widget "increaseQuantity"
    (Widget.button "+")
    <&> whenAlt (ChangeBuildingQueueQuantity <$> selectedTaskIndex <*> pure 1)
    <&> join

  decreaseQuantity <- Updating.widget "decreaseQuantity"
    (Widget.button "−")
    <&> whenAlt (ChangeBuildingQueueQuantity <$> selectedTaskIndex <*> pure (-1))
    <&> join

  moveUp <- Updating.widget "moveUp"
    (Widget.button "▲")
    <&> whenAlt (MoveUpInBuildingQueue <$> selectedTaskIndex)
    <&> join

  moveDown <- Updating.widget "moveDown"
    (Widget.button "▼")
    <&> whenAlt (MoveDownInBuildingQueue <$> selectedTaskIndex)
    <&> join

  let toggleInstallLabel =
        case selectedTask ^? _Just . #installWhenDone of
          Just False -> "Install after"
          _ -> "Do not install"
  toggleInstall <- Updating.widget "toggleInstall"
    (Widget.button toggleInstallLabel)
    <&> whenAlt (ToggleInstallInBuildingQueue <$> selectedTaskIndex)
    <&> join

  pure (enqueue <|> increaseQuantity <|> decreaseQuantity <|> moveUp <|> moveDown <|> toggleInstall)
