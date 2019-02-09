module App.UI.ProductionWindow
  ( update )
where

import App.Prelude

import qualified App.Common.IdMap as IdMap
import qualified App.Common.Print as Print
import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Logic.Building as Logic.Building
import qualified App.Logic.Mining as Logic.Mining
import qualified App.Logic.ShipBuilding as Logic.ShipBuilding
import qualified App.Model.BuildTask as BuildTask
import qualified App.Model.Mineral as Mineral
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Update.ListBoxState as ListBoxState
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified App.Update.WidgetTree as WidgetTree

import App.Common.Util (boolToMaybe)
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

data Action
  = ChangeMiningPriority Resource Int
  | EnqueueBuildingTask Installation
  | ChangeBuildingQueueQuantity Int Int
  | MoveUpInBuildingQueue Int
  | MoveDownInBuildingQueue Int
  | ToggleInstallInBuildingQueue Int

update :: GameState -> Updating GameState
update gs@GameState{ bodies, colonies, bodyMinerals, time } =
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
            Updating.widget "mineCountLabel" $ Widget.label' "Mines"
            Updating.widget "mineCount" $
              Widget.label (Print.int mines)

            let factories = installations ^. at Installation.Factory . non 0
            Updating.widget "factoryCountLabel" $ Widget.label' "Factories"
            Updating.widget "factoryCount" $
              Widget.label (Print.int factories)

            let shipyards = installations ^. at Installation.Shipyard . non 0
            Updating.widget "shipyardCountLabel" $ Widget.label' "Shipyard"
            Updating.widget "shipyardCount" $
              Widget.label (Print.int shipyards)

          miningAction <- Updating.useWidget "mining" $
            miningPanel colony minerals
          buildingAction <- Updating.useWidget "building" $
            buildingPanel time colony
          shipBuildingAction <- Updating.useWidget "shipBuilding" $
            shipBuildingPanel time colony

          case miningAction <|> buildingAction <|> shipBuildingAction of
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
    Updating.widget "title" $ Widget.label' "Mines"
    Updating.widget "totalMined" $
      Widget.label ("Mined / mo at 100% acc.: " <> Print.float0 totalMonthlyMined <> " t")
    Updating.thisWidget Widget.bottomLine

  Updating.useWidget "headerRow" $ do
    Updating.widget "monthlyMined" $ Widget.label' "Mined / mo"
    Updating.widget "priority" $ Widget.label' "Priority"
    Updating.widget "mineable" $ Widget.label' "Mineable"
    Updating.widget "accessibility" $ Widget.label' "Accessibility"
    Updating.widget "stockpile" $ Widget.label' "Stockpile"

  Updating.useWidget "mineralRows" $ do
    let allMonthlyMined = Time.daysInMonth *^ Logic.Mining.dailyMined colony minerals
    actions <- ifor Resource.minerals $ \i resource -> do
      let Mineral{ available, accessibility } = minerals ^. at resource . Mineral.nonEmpty
          monthlyMined = allMonthlyMined ^. at resource . non 0
          priority = miningPriorities ^. at resource . non 0
          inStockpile = stockpile ^. at resource . non 0
      Updating.useWidget "row" $ do
        rowHeight <- view (#bounds . #wh . _y) <$> Updating.thisWidget pure
        let offsetRow bounds = bounds & #xy . _y +~ i * rowHeight
        -- TODO this should be a table widget
        local (#resources . #widgetTree %~ WidgetTree.mapTree (#bounds %~ offsetRow)) $ do
          Updating.widget "name" $
            Widget.label (Resource.print resource)
          Updating.widget "monthlyMined" $
            Widget.label (Print.float0 monthlyMined <> " t")
          Updating.widget "mineable" $
            Widget.label (Print.float0 available <> " t")
          Updating.widget "accessibility" $
            Widget.label (Print.float0 (100 * accessibility) <> "%")
          Updating.widget "stockpile" $
            Widget.label (Print.float0 inStockpile)
          Updating.widget "priority" $
            Widget.label (Print.int priority)

          increasePriority <- Updating.widget "increasePriority"
            (Widget.button "+")
            <&> boolToMaybe (ChangeMiningPriority resource 1)

          decreasePriority <- Updating.widget "decreasePriority"
            (Widget.button "−")
            <&> boolToMaybe (ChangeMiningPriority resource (-1))

          pure (increasePriority <|> decreasePriority)

    pure (asum actions)

buildingPanel :: Time Int -> Colony -> Updating (Maybe Action)
buildingPanel now colony@Colony{ buildQueue } = do
  Updating.useWidget "headingRow" $ do
    let monthlyBuildEffort = Time.daysInMonth * Logic.Building.dailyBuildEffort colony
    Updating.widget "title" $ Widget.label' "Factories"
    Updating.widget "monthlyBuildEffort" $
      Widget.label ("Build effort / mo: " <> Print.int monthlyBuildEffort)
    Updating.thisWidget Widget.bottomLine

  selectedInstallation <- Updating.widget "selectedInstallation" $
    Widget.closedDropdown
      20 380
      id (Installation.print >>> Print.toText)
      #selectedInstallation
      Installation.all

  enqueue <- Updating.widget "enqueue"
    (Widget.button "Enqueue")
    <&> boolToMaybe (EnqueueBuildingTask <$> selectedInstallation)
    <&> join

  reset <- Updating.widget "reset" $
    Widget.button "Reset"
  when reset (#ui . #selectedInstallation . #selectedIndex .= Nothing)

  for_ selectedInstallation $ \installation -> do
    let buildEffort = Logic.Building.buildEffortNeeded installation
        done = Logic.Building.finishTime now 0 installation colony
        cost = Logic.Building.resourcesNeeded installation
    Updating.widget "newBuildEffort" $
      Widget.label ("Build effort: " <> Print.int buildEffort)
    Updating.widget "newDone" $
      Widget.label ("Done: " <> Time.printDate done)
    Updating.widget "newCost" $
      Widget.label ("Cost: " <> Resource.printCost cost)

  (selectedTaskWithIndex, _) <- Updating.widget "buildQueue" $
    Widget.listBox
      20 (view _1) (view (_2 . to BuildTask.print . to Print.toText))
      #selectedBuildingTaskIndex
      (zip [0..] buildQueue)
  let (selectedTaskIndex, selectedTask) = munzip selectedTaskWithIndex

  for_ selectedTask $ \BuildTask{ installation, buildEffortSpent } -> do
    let buildEffort = Logic.Building.buildEffortNeeded installation
        done = Logic.Building.finishTime now buildEffortSpent installation colony
        cost = Logic.Building.resourcesNeeded installation
        doneLabel = if 0 `elem` selectedTaskIndex then "Done: " else "Done if first: "
    Updating.widget "enqueuedBuildEffort" $
      Widget.label ("Build effort: " <> Print.int buildEffortSpent <> " / " <> Print.int buildEffort)
    Updating.widget "enqueuedDone" $
      Widget.label (doneLabel <> Time.printDate done)
    Updating.widget "enqueuedCost" $
      Widget.label ("Cost: " <> Resource.printCost cost)

  increaseQuantity <- Updating.widget "increaseQuantity"
    (Widget.button "+")
    <&> boolToMaybe (ChangeBuildingQueueQuantity <$> selectedTaskIndex <*> pure 1)
    <&> join

  decreaseQuantity <- Updating.widget "decreaseQuantity"
    (Widget.button "−")
    <&> boolToMaybe (ChangeBuildingQueueQuantity <$> selectedTaskIndex <*> pure (-1))
    <&> join

  moveUp <- Updating.widget "moveUp"
    (Widget.button "▲")
    <&> boolToMaybe (MoveUpInBuildingQueue <$> selectedTaskIndex)
    <&> join

  moveDown <- Updating.widget "moveDown"
    (Widget.button "▼")
    <&> boolToMaybe (MoveDownInBuildingQueue <$> selectedTaskIndex)
    <&> join

  let toggleInstallLabel =
        case selectedTask ^? _Just . #installWhenDone of
          Just False -> "Install after"
          _ -> "Do not install"
  toggleInstall <- Updating.widget "toggleInstall"
    (Widget.button toggleInstallLabel)
    <&> boolToMaybe (ToggleInstallInBuildingQueue <$> selectedTaskIndex)
    <&> join

  pure (enqueue <|> increaseQuantity <|> decreaseQuantity <|> moveUp <|> moveDown <|> toggleInstall)

shipBuildingPanel :: Time Int -> Colony -> Updating (Maybe Action)
shipBuildingPanel now colony = do
  Updating.useWidget "headingRow" $ do
    let monthlyBuildEffort = Time.daysInMonth * Logic.ShipBuilding.dailyBuildEffort colony
    Updating.widget "title" $ Widget.label' "Shipyards"
    Updating.widget "monthlyBuildEffort" $
      Widget.label ("Build effort / mo: " <> Print.int monthlyBuildEffort)
    Updating.thisWidget Widget.bottomLine

  selectedShipType <- Updating.widget "selectedShipType" $
    Widget.closedDropdown
      20 380
      id Ship.printType
      #selectedShipType
      Ship.types

  shipSize <- use (#ui . #selectedShipSize)
  Updating.widget "shipSizeLabel" (Widget.label' "Size:")
  Updating.widget "shipSize" (Widget.label (Print.int shipSize))

  Updating.widget "increaseShipSize" (Widget.button "+") >>= \click ->
    when click $
      #ui . #selectedShipSize %= ((+ 1) >>> min 10)

  Updating.widget "decreaseShipSize" (Widget.button "-") >>= \click ->
    when click $
      #ui . #selectedShipSize %= (subtract 1 >>> max 1)

  build <- Updating.widget "build"
    (Widget.button "Build")
    <&> boolToMaybe (Just ())
    <&> join

  Updating.widget "reset" (Widget.button "Reset") >>= \click ->
    when click $ do
      #ui . #selectedShipType .= ListBoxState.initial
      #ui . #selectedShipSize .= 1

  for_ selectedShipType $ \shipType -> do
    let capability =
          case Logic.ShipBuilding.capabilityOf shipType shipSize of
            Ship.Freighter Ship.FreighterCapability{ cargoCapacity } ->
              "Cargo capacity: " <> Print.float0 cargoCapacity <> " t"
            Ship.ColonyShip Ship.ColonyShipCapability { cabinCapacity } ->
              "Cabin capacity: " <> Print.int cabinCapacity
        speed = "Speed: " <> Speed.printKmPerSec (Logic.ShipBuilding.speedOf shipSize)
        buildEffort = "Build effort: " <> Print.int (Logic.ShipBuilding.buildEffortNeeded shipSize)
        done = "Done: " <> Time.printDate (Logic.ShipBuilding.finishTime now 0 shipSize colony)
        cost = "Cost: " <> Resource.printCost (Logic.ShipBuilding.resourcesNeeded shipSize)
    Updating.widget "capability" (Widget.label capability)
    Updating.widget "speed" (Widget.label speed)
    Updating.widget "buildEffort" (Widget.label buildEffort)
    Updating.widget "done" (Widget.label done)
    Updating.widget "cost" (Widget.label cost)

  pure Nothing
