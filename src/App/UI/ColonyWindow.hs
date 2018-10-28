module App.UI.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Installation as Installation
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text

import App.Common.Id (Id)
import App.Common.Util (whenAlt)
import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Update.Updating (Updating)
import Data.String (fromString)
import Text.Read (readMaybe)

data Action
  = BuildShip
  | CancelBuildingShip
  | Install Installation Int Colony
  | Uninstall Installation Int Colony
  | FoundColony

update :: GameState -> Updating GameState
update gs = do
  Updating.useWidget "colonyWindow" $ do
    Updating.widget "decoration" $
      Widget.window 20 "Colonies"

    (selectedBody, _) <-
      Updating.widget "selectedBody" $
        Widget.listBox
          20 (view #bodyId) (view #name)
          #selectedBody
          (gs ^.. #bodies . folded)
    gs' <- Updating.useWidget "rightPanel" $
      for selectedBody $ \body@Body{ bodyId, colonyCost } -> do
        Updating.widget "info" $
          Widget.labels 20
            [ case colonyCost of
                Just cc -> fromString $ printf "Colony cost: ×%.1f" cc
                Nothing -> "Uncolonizable"
            ]
        Updating.useWidget "mineralTable" $
          mineralTable bodyId gs

        action <- case gs ^. #colonies . at bodyId of
          Just colony@Colony{ population } -> do
            Updating.useWidget "stockpileTable" $
              stockpileTable colony
            Updating.useWidget "installationTable" $
              installationTable colony
            Updating.widget "populationInfo" $
              Widget.labels 20
                [ fromString $ printf "Population: %d" population
                , case Logic.Colony.colonyMaxPopulation body colony of
                    Just mp -> fromString $ printf "Max. population: %d" mp
                    Nothing -> "Max. population: ∞"
                ]

            buildShip <- Updating.useWidget "shipBuildingPanel" $
              shipBuildingPanel colony
            install <- Updating.useWidget "installationPanel" $
              installationPanel colony

            pure (buildShip <|> install)
          Nothing ->
            case colonyCost of
              Just _ ->
                Updating.widget "foundColony"
                  (Widget.button "Found colony")
                  <&> whenAlt FoundColony
              Nothing -> pure Nothing

        pure $ case action of
          Just BuildShip -> Logic.Colony.startShipBuildingTask bodyId gs
          Just CancelBuildingShip -> Logic.Colony.cancelShipBuildingTask bodyId gs
          Just (Install installation qty colony) -> Logic.Colony.installInstallation installation qty colony gs
          Just (Uninstall installation qty colony) -> Logic.Colony.uninstallInstallation installation qty colony gs
          Just FoundColony -> Logic.Colony.foundColony bodyId gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

mineralTable :: Id Body -> GameState -> Updating ()
mineralTable bodyId gs = do
  let minerals = gs ^@.. #bodyMinerals . at bodyId . _Just . ifolded
      (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
        minerals <&> \(mineral, Mineral{ available, accessibility }) ->
          ( fromString $ show mineral
          , fromString $ printf "%.0f t" available
          , fromString $ printf "%.0f%%" (100 * accessibility)
          ) -- TODO table widget?
  Updating.widget "title" $ Widget.label "Mineable resources"
  Updating.widget "minerals" $ Widget.labels 20 mineralLabels
  Updating.widget "availables" $ Widget.labels 20 availableLabels
  Updating.widget "accessibilities" $ Widget.labels 20 accessibilityLabels

stockpileTable :: Colony -> Updating ()
stockpileTable Colony{ stockpile } = do
  let (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(resource, mass) ->
        ( fromString $ show resource
        , fromString $ if resource & has (_Ctor @"Installation")
          then printf "%.0f t (%d buildings)" mass (floor (mass / Installation.mass) :: Int)
          else printf "%.0f t" mass
        )
  Updating.widget "title" $ Widget.label "Resource stockpile"
  Updating.widget "resources" $ Widget.labels 20 itemLabels
  Updating.widget "quantities" $ Widget.labels 20 qtyLabels

installationTable :: Colony -> Updating ()
installationTable Colony{ installations } = do
  let (mineLabels, mineQtyLabels) = unzip $ itoList installations <&> \(installation, qty) ->
        ( fromString $ show installation
        , fromString $ show qty
        )
  Updating.widget "title" $ Widget.label "Installations"
  Updating.widget "installations" $ Widget.labels 20 mineLabels
  Updating.widget "quantities" $ Widget.labels 20 mineQtyLabels

shipBuildingPanel :: Colony -> Updating (Maybe Action)
shipBuildingPanel Colony{ shipBuildingTask } = do
  let label = case shipBuildingTask of
        Just ShipBuildingTask{} -> "Producing: Ship"
        Nothing -> "Producing: nothing"
  Updating.widget "label" $ Widget.label label

  build <- Updating.widget "build"
    (Widget.button "Produce ship")
    <&> whenAlt BuildShip

  cancel <- Updating.widget "cancel"
    (Widget.button "Cancel")
    <&> whenAlt CancelBuildingShip

  pure (build <|> cancel)

installationPanel :: Colony -> Updating (Maybe Action)
installationPanel colony = do
  selectedInstallation <- Updating.widget "selectedInstallation" $
    Widget.closedDropdown
      20 380
      id (show >>> fromString)
      #selectedInstallation
      Installation.all

  Updating.widget "qtyLabel" $ Widget.label "Qty:"

  qty <- Updating.widget "qty"
    (Widget.textBox #editedInstallationQty)
    <&> (Text.unpack >>> readMaybe @Int >>> mfilter (>= 0))

  install <- Updating.widget "install"
    (Widget.button "Install")
    <&> whenAlt (Install <$> selectedInstallation <*> qty <*> pure colony)
    <&> join

  uninstall <- Updating.widget "uninstall"
    (Widget.button "Uninstall")
    <&> whenAlt (Uninstall <$> selectedInstallation <*> qty <*> pure colony)
    <&> join

  pure (install <|> uninstall)
