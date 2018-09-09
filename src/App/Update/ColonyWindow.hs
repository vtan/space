module App.Update.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Update.Logic as Logic
import qualified App.Update.Widget as Widget

import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.BodyMinerals (MineralData(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Rect (Rect(..))
import App.Update.Updating (Updating)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs = do
  Widget.window (Rect (V2 32 32) (V2 (4 + 200 + 4 + 256 + 4) (500 + 24))) 20 "Colonies"
  let p = V2 36 56

  (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
    (Rect p (V2 200 500)) 20
    (view #uid) (view #name)
    (gs ^.. #bodies . folded)
  for_ clickedBody $ \Body{ Body.uid } ->
    #ui . #selectedBodyUid .= Just uid

  gsMay' <- for selectedBody $ \Body{ Body.uid } -> do
    let minerals = gs ^@.. #bodyMinerals . at uid . _Just . ifolded
        (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
          minerals <&> \(mineral, MineralData{ available, accessibility }) ->
            ( fromString $ printf "Mineral #%d" mineral
            , fromString $ printf "%.2f t" available
            , fromString $ printf "%.0f%%" (100 * accessibility)
            ) -- TODO table widget?
    Widget.label (p + V2 (200 + 4) 0) "Mineable resources"
    Widget.labels (p + V2 (200 + 4) 20) 20 mineralLabels
    Widget.labels (p + V2 (200 + 4 + 100) 20) 20 availableLabels
    Widget.labels (p + V2 (200 + 4 + 180) 20) 20 accessibilityLabels

    let q = p + V2 (200 + 4) (length minerals * 20 + 28)
    case gs ^. #colonies . at uid of
      Just Colony{ stockpile, mines, buildingTask, shipBuildingTask } -> do
        Widget.label q "Resource stockpile"
        let (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(mineral, qty) ->
              ( fromString $ printf "Mineral #%d" mineral
              , fromString $ printf "%.2f t" qty
              )
        Widget.labels (q + V2 0 20) 20 itemLabels
        Widget.labels (q + V2 100 20) 20 qtyLabels

        let (mineLabels, mineQtyLabels) = unzip $ itoList mines <&> \(mineral, mineQty) ->
              ( fromString $ printf "Mineral #%d" mineral
              , fromString $ printf "%d mines" mineQty
              )
            r = q + V2 0 (length stockpile * 20 + 28)
        Widget.label r "Mining"
        Widget.labels (r + V2 0 20) 20 mineLabels
        Widget.labels (r + V2 100 20) 20 mineQtyLabels

        let s = r + V2 0 (length mines * 20 + 8)
        case buildingTask of
          Just BuildingTask{ minedMineral } ->
            Widget.label (s + V2 0 20) (fromString $ printf "Building: Mine for Mineral #%d" minedMineral)
          Nothing ->
            Widget.label (s + V2 0 20) "Building: nothing"
        buildNewMine <- Widget.button (Rect (s + V2 0 40) (V2 256 20)) "Build mine for Mineral #0"

        case shipBuildingTask of
          Just ShipBuildingTask{} -> Widget.label (s + V2 0 68) "Producing: Ship"
          Nothing -> Widget.label (s + V2 0 68) "Producing: nothing"
        buildNewShip <- Widget.button (Rect (s + V2 0 88) (V2 256 20)) "Produce ship"

        pure $ if
          | buildNewMine -> Logic.startBuildingTask uid 0 gs
          | buildNewShip -> Logic.startShipBuildingTask uid gs
          | otherwise -> gs
      Nothing -> do
        found <- Widget.button (Rect q (V2 128 20)) "Found colony"
        pure $ if found
          then Logic.foundColony uid gs
          else gs
  pure $ fromMaybe gs gsMay'
