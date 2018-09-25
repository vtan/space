module App.Update.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Model.Resource as Resource
import qualified App.Update.Logic as Logic
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget

import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Rect (Rect(..))
import App.Uid (Uid)
import App.Update.Updating (Updating)
import App.Util (whenAlt)
import Data.String (fromString)

data Action
  = BuildMine
  | BuildShip
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
      for selectedBody $ \Body{ uid } -> do
        Updating.childBounds "mineralTable" $ \bounds ->
          mineralTable bounds uid gs

        action <- case gs ^. #colonies . at uid of
          Just colony -> do
            Updating.childBounds "stockpileTable" $ \bounds ->
              stockpileTable bounds colony
            Updating.childBounds "mineTable" $ \bounds ->
              mineTable bounds colony

            buildMine <- Updating.childBounds "buildingPanel" $ \bounds ->
              buildingPanel bounds colony
            buildShip <- Updating.childBounds "shipBuildingPanel" $ \bounds ->
              shipBuildingPanel bounds colony
            pure (buildMine <|> buildShip)
          Nothing ->
            Updating.childBounds "foundColony" $ \bounds ->
              Widget.button bounds "Found colony"
              <&> whenAlt FoundColony

        pure $ case action of
          Just BuildMine -> Logic.startBuildingTask uid Resource.Mineral gs
          Just BuildShip -> Logic.startShipBuildingTask uid gs
          Just FoundColony -> Logic.foundColony uid gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

mineralTable :: Rect Int -> Uid Body -> GameState -> Updating ()
mineralTable bounds bodyUid gs =
  let p = bounds ^. #xy
      minerals = gs ^@.. #bodyMinerals . at bodyUid . _Just . ifolded
      (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
        minerals <&> \(mineral, Mineral{ available, accessibility }) ->
          ( fromString $ show mineral
          , fromString $ printf "%.2f t" available
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
        , fromString $ printf "%.2f t" qty
        )
  in do
    Widget.label p "Resource stockpile"
    Widget.labels (p + V2 0 20) 20 itemLabels
    Widget.labels (p + V2 100 20) 20 qtyLabels

mineTable :: Rect Int -> Colony -> Updating ()
mineTable bounds Colony{ mines } =
  let p = bounds ^. #xy
      (mineLabels, mineQtyLabels) = unzip $ itoList mines <&> \(mineral, mineQty) ->
        ( fromString $ show mineral
        , fromString $ printf "%d mines" mineQty
        )
  in do
    Widget.label p "Mining"
    Widget.labels (p + V2 0 20) 20 mineLabels
    Widget.labels (p + V2 100 20) 20 mineQtyLabels

buildingPanel :: Rect Int -> Colony -> Updating (Maybe Action)
buildingPanel bounds Colony{ buildingTask } = do
  let p = bounds ^. #xy
  case buildingTask of
    Just BuildingTask{ minedMineral } ->
      Widget.label p (fromString $ printf "Building: Mine for %s" (show minedMineral))
    Nothing ->
      Widget.label p "Building: nothing"
  Widget.button (Rect (p + V2 0 20) (V2 256 20)) "Build mine for Mineral"
    <&> whenAlt BuildMine

shipBuildingPanel :: Rect Int -> Colony -> Updating (Maybe Action)
shipBuildingPanel bounds Colony{ shipBuildingTask } = do
  let p = bounds ^. #xy
  case shipBuildingTask of
    Just ShipBuildingTask{} -> Widget.label p "Producing: Ship"
    Nothing -> Widget.label p "Producing: nothing"
  Widget.button (Rect (p + V2 0 20) (V2 256 20)) "Produce ship"
    <&> whenAlt BuildShip
