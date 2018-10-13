module App.UI.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Installation as Installation
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text

import App.Common.Rect (Rect(..))
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
          Widget.labels (bounds ^. #xy) 20
            [ case colonyCost of
                Just cc -> fromString $ printf "Colony cost: ×%.1f" cc
                Nothing -> "Uncolonizable"
            ]
        Updating.childBounds "mineralTable" $ \bounds ->
          mineralTable bounds uid gs

        action <- case gs ^. #colonies . at uid of
          Just colony@Colony{ population } -> do
            Updating.childBounds "stockpileTable" $ \bounds ->
              stockpileTable bounds colony
            Updating.childBounds "installationTable" $ \bounds ->
              installationTable bounds colony
            Updating.childBounds "populationInfo" $ \bounds ->
              Widget.labels (bounds ^. #xy) 20
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
          Just (Install installation qty colony) -> Logic.Colony.installInstallation installation qty uid colony gs
          Just (Uninstall installation qty colony) -> Logic.Colony.uninstallInstallation installation qty uid colony gs
          Just FoundColony -> Logic.Colony.foundColony uid gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

mineralTable :: Rect Int -> Uid Body -> GameState -> Updating ()
mineralTable bounds bodyUid gs =
  let p = bounds ^. #xy
      minerals = gs ^@.. #bodyMinerals . at bodyUid . _Just . ifolded
      (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
        minerals <&> \(mineral, Mineral{ available, accessibility }) ->
          ( fromString $ show mineral
          , fromString $ printf "%d t" available
          , fromString $ printf "%.0f%%" (100 * accessibility)
          ) -- TODO table widget?
  in do
    Widget.label p "Mineable resources"
    Widget.labels (p + V2 0 20) 20 mineralLabels
    Widget.labels (p + V2 100 20) 20 availableLabels
    Widget.labels (p + V2 180 20) 20 accessibilityLabels

stockpileTable :: Rect Int -> Colony -> Updating ()
stockpileTable bounds Colony{ stockpile } =
  let p = bounds ^. #xy
      (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(mineral, qty) ->
        ( fromString $ show mineral
        , fromString $ printf "%d t" qty
        )
  in do
    Widget.label p "Resource stockpile"
    Widget.labels (p + V2 0 20) 20 itemLabels
    Widget.labels (p + V2 180 20) 20 qtyLabels

installationTable :: Rect Int -> Colony -> Updating ()
installationTable bounds Colony{ installations } =
  let p = bounds ^. #xy
      (mineLabels, mineQtyLabels) = unzip $ itoList installations <&> \(installation, qty) ->
        ( fromString $ show installation
        , fromString $ printf "%d t" qty
        )
  in do
    Widget.label p "Installations"
    Widget.labels (p + V2 0 20) 20 mineLabels
    Widget.labels (p + V2 180 20) 20 mineQtyLabels

buildingPanel :: Colony -> Updating (Maybe Action)
buildingPanel Colony{ buildingTask } = do
  Updating.childBounds "label" $ \bounds -> do
    let p = bounds ^. #xy
    case buildingTask of
      Just BuildingTask{ installation, quantity } ->
        Widget.label p (fromString $ printf "Building: %s (%d t)" (show installation) quantity)
      Nothing ->
        Widget.label p "Building: nothing"

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
    let p = bounds ^. #xy
    case shipBuildingTask of
      Just ShipBuildingTask{} -> Widget.label p "Producing: Ship"
      Nothing -> Widget.label p "Producing: nothing"

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
    Widget.label (bounds ^. #xy) "Qty:"

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
