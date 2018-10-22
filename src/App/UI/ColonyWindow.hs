module App.UI.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Installation as Installation
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text

import App.Common.Uid (Uid)
import App.Common.Util (whenAlt)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Update.Updating (Updating)
import Data.String (fromString)
import Text.Read (readMaybe)

data Action
  = Build Installation
  | BuildShip
  | CancelBuilding
  | CancelBuildingShip
  | Install Installation Int Colony
  | Uninstall Installation Int Colony
  | FoundColony

update :: GameState -> Updating GameState
update gs = do
  Updating.childLayout "colonyWindow" $ do
    Updating.childBounds "decoration" $ \bounds ->
      Widget.window bounds 20 "Colonies"

    (selectedBody, _) <-
      Updating.childBounds "selectedBody" $ \bounds ->
        Widget.listBox
          bounds 20
          (view #uid) (view #name)
          #selectedBody
          (gs ^.. #bodies . folded)
    gs' <- Updating.childLayout "rightPanel" $
      for selectedBody $ \body@Body{ uid, colonyCost } -> do
        Updating.childBounds "info" $ \bounds ->
          Widget.labels bounds 20
            [ case colonyCost of
                Just cc -> fromString $ printf "Colony cost: ×%.1f" cc
                Nothing -> "Uncolonizable"
            ]
        Updating.childLayout "mineralTable" $
          mineralTable uid gs

        action <- case gs ^. #colonies . at uid of
          Just colony@Colony{ population } -> do
            Updating.childLayout "stockpileTable" $
              stockpileTable colony
            Updating.childLayout "installationTable" $
              installationTable colony
            Updating.childBounds "populationInfo" $ \bounds ->
              Widget.labels bounds 20
                [ fromString $ printf "Population: %d" population
                , case Logic.Colony.colonyMaxPopulation body colony of
                    Just mp -> fromString $ printf "Max. population: %d" mp
                    Nothing -> "Max. population: ∞"
                ]

            buildMine <- Updating.childLayout "buildingPanel" $
              buildingPanel colony
            buildShip <- Updating.childLayout "shipBuildingPanel" $
              shipBuildingPanel colony
            install <- Updating.childLayout "installationPanel" $
              installationPanel colony

            pure (buildMine <|> buildShip <|> install)
          Nothing ->
            case colonyCost of
              Just _ ->
                Updating.childBounds "foundColony" $ \bounds ->
                  Widget.button bounds "Found colony"
                  <&> whenAlt FoundColony
              Nothing -> pure Nothing

        pure $ case action of
          Just (Build installation) -> Logic.Colony.startBuildingTask uid installation gs
          Just BuildShip -> Logic.Colony.startShipBuildingTask uid gs
          Just CancelBuilding -> Logic.Colony.cancelBuildingTask uid gs
          Just CancelBuildingShip -> Logic.Colony.cancelShipBuildingTask uid gs
          Just (Install installation qty colony) -> Logic.Colony.installInstallation installation qty colony gs
          Just (Uninstall installation qty colony) -> Logic.Colony.uninstallInstallation installation qty colony gs
          Just FoundColony -> Logic.Colony.foundColony uid gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

mineralTable :: Uid Body -> GameState -> Updating ()
mineralTable bodyUid gs = do
  let minerals = gs ^@.. #bodyMinerals . at bodyUid . _Just . ifolded
      (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
        minerals <&> \(mineral, Mineral{ available, accessibility }) ->
          ( fromString $ show mineral
          , fromString $ printf "%.0f t" available
          , fromString $ printf "%.0f%%" (100 * accessibility)
          ) -- TODO table widget?
  Updating.childBounds "title" $ \bounds ->
    Widget.label bounds "Mineable resources"
  Updating.childBounds "minerals" $ \bounds ->
    Widget.labels bounds 20 mineralLabels
  Updating.childBounds "availables" $ \bounds ->
    Widget.labels bounds 20 availableLabels
  Updating.childBounds "accessibilities" $ \bounds ->
    Widget.labels bounds 20 accessibilityLabels

stockpileTable :: Colony -> Updating ()
stockpileTable Colony{ stockpile } = do
  let (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(resource, mass) ->
        ( fromString $ show resource
        , fromString $ if resource & has (_Ctor @"Installation")
          then printf "%.0f t (%d buildings)" mass (floor (mass / Installation.mass) :: Int)
          else printf "%.0f t" mass
        )
  Updating.childBounds "title" $ \bounds ->
    Widget.label bounds "Resource stockpile"
  Updating.childBounds "resources" $ \bounds ->
    Widget.labels bounds 20 itemLabels
  Updating.childBounds "quantities" $ \bounds ->
    Widget.labels bounds 20 qtyLabels

installationTable :: Colony -> Updating ()
installationTable Colony{ installations } = do
  let (mineLabels, mineQtyLabels) = unzip $ itoList installations <&> \(installation, qty) ->
        ( fromString $ show installation
        , fromString $ show qty
        )
  Updating.childBounds "title" $ \bounds ->
    Widget.label bounds "Installations"
  Updating.childBounds "installations" $ \bounds ->
    Widget.labels bounds 20 mineLabels
  Updating.childBounds "quantities" $ \bounds ->
    Widget.labels bounds 20 mineQtyLabels

buildingPanel :: Colony -> Updating (Maybe Action)
buildingPanel Colony{ buildingTask } = do
  Updating.childBounds "label" $ \bounds -> do
    case buildingTask of
      Just BuildingTask{ installation, quantity } ->
        Widget.label bounds (fromString $ printf "Building: %s (%d)" (show installation) quantity)
      Nothing ->
        Widget.label bounds "Building: nothing"

  selectedInstallation <- Updating.childBounds "selectedInstallation" $ \bounds ->
    Widget.closedDropdown
      bounds 20 380
      id (show >>> fromString)
      #selectedInstallation
      Installation.all

  build <- Updating.childBounds "build" $ \bounds -> do
    Widget.button bounds "Build"
      <&> whenAlt (Build <$> selectedInstallation)
      <&> join

  cancel <- Updating.childBounds "cancel" $ \bounds -> do
    Widget.button bounds "Cancel"
      <&> whenAlt CancelBuilding

  pure (build <|> cancel)

shipBuildingPanel :: Colony -> Updating (Maybe Action)
shipBuildingPanel Colony{ shipBuildingTask } = do
  Updating.childBounds "label" $ \bounds -> do
    case shipBuildingTask of
      Just ShipBuildingTask{} -> Widget.label bounds "Producing: Ship"
      Nothing -> Widget.label bounds "Producing: nothing"

  build <- Updating.childBounds "build" $ \bounds -> do
    Widget.button bounds "Produce ship"
      <&> whenAlt BuildShip

  cancel <- Updating.childBounds "cancel" $ \bounds -> do
    Widget.button bounds "Cancel"
      <&> whenAlt CancelBuildingShip

  pure (build <|> cancel)

installationPanel :: Colony -> Updating (Maybe Action)
installationPanel colony = do
  selectedInstallation <- Updating.childBounds "selectedInstallation" $ \bounds ->
    Widget.closedDropdown
      bounds 20 380
      id (show >>> fromString)
      #selectedInstallation
      Installation.all

  Updating.childBounds "qtyLabel" $ \bounds ->
    Widget.label bounds "Qty:"

  qty <- Updating.childBounds "qty" $ \bounds ->
    Widget.textBox "installationQty" bounds #editedInstallationQty
      <&> (Text.unpack >>> readMaybe @Int >>> mfilter (>= 0))

  install <- Updating.childBounds "install" $ \bounds ->
    Widget.button bounds "Install"
      <&> whenAlt (Install <$> selectedInstallation <*> qty <*> pure colony)
      <&> join

  uninstall <- Updating.childBounds "uninstall" $ \bounds ->
    Widget.button bounds "Uninstall"
      <&> whenAlt (Uninstall <$> selectedInstallation <*> qty <*> pure colony)
      <&> join

  pure (install <|> uninstall)
