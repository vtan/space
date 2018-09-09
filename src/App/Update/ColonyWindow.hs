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
  Widget.window (Rect (V2 32 32) (V2 (4 + 200 + 4 + 256 + 4) (500 + 24))) 20 "Colonies"
  let p = V2 36 56

  selectedBody <- bodyList p gs
  gs' <- for selectedBody $ \Body{ uid } -> do
    mineralTableHeight <- mineralTable (p + V2 (200 + 4) 0) uid gs
    let q = p + V2 (200 + 4) (mineralTableHeight + 8)

    action <- case gs ^. #colonies . at uid of
      Just colony -> do
        stockpileTableHeight <- stockpileTable q colony
        mineTableHeight <- mineTable (q + V2 0 (stockpileTableHeight + 8)) colony

        let r = q + V2 0 (stockpileTableHeight + 8 + mineTableHeight + 8)
        buildMine <- buildingPanel r colony
        buildShip <- shipBuildingPanel (r + V2 0 48) colony
        pure (buildMine <|> buildShip)
      Nothing ->
        Widget.button (Rect q (V2 128 20)) "Found colony"
          <&> whenAlt FoundColony

    pure $ case action of
      Just BuildMine -> Logic.startBuildingTask uid 0 gs
      Just BuildShip -> Logic.startShipBuildingTask uid gs
      Just FoundColony -> Logic.foundColony uid gs
      Nothing -> gs

  pure (gs' & fromMaybe gs)

bodyList :: V2 Int -> GameState -> Updating (Maybe Body)
bodyList p gs = do
  (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
    (Rect p (V2 200 500)) 20
    (view #uid) (view #name)
    (gs ^.. #bodies . folded)
  for_ clickedBody $ \Body{ Body.uid } ->
    #ui . #selectedBodyUid .= Just uid
  pure selectedBody

mineralTable :: V2 Int -> Uid Body -> GameState -> Updating Int
mineralTable p bodyUid gs =
  let minerals = gs ^@.. #bodyMinerals . at bodyUid . _Just . ifolded
      (mineralLabels, availableLabels, accessibilityLabels) = unzip3 $
        minerals <&> \(mineral, MineralData{ available, accessibility }) ->
          ( fromString $ printf "Mineral #%d" mineral
          , fromString $ printf "%.2f t" available
          , fromString $ printf "%.0f%%" (100 * accessibility)
          ) -- TODO table widget?
  in do
    Widget.label p "Mineable resources"
    Widget.labels (p + V2 0 20) 20 mineralLabels
    Widget.labels (p + V2 100 20) 20 availableLabels
    Widget.labels (p + V2 180 20) 20 accessibilityLabels
    pure $ (1 + length minerals) * 20

stockpileTable :: V2 Int -> Colony -> Updating Int
stockpileTable p Colony{ stockpile } =
  let (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(mineral, qty) ->
        ( fromString $ printf "Mineral #%d" mineral
        , fromString $ printf "%.2f t" qty
        )
  in do
    Widget.label p "Resource stockpile"
    Widget.labels (p + V2 0 20) 20 itemLabels
    Widget.labels (p + V2 100 20) 20 qtyLabels
    pure $ (1 + length stockpile) * 20

mineTable :: V2 Int -> Colony -> Updating Int
mineTable p Colony{ mines } =
  let (mineLabels, mineQtyLabels) = unzip $ itoList mines <&> \(mineral, mineQty) ->
        ( fromString $ printf "Mineral #%d" mineral
        , fromString $ printf "%d mines" mineQty
        )
  in do
    Widget.label p "Mining"
    Widget.labels (p + V2 0 20) 20 mineLabels
    Widget.labels (p + V2 100 20) 20 mineQtyLabels
    pure $ (1 + length mines) * 20

buildingPanel :: V2 Int -> Colony -> Updating (Maybe Action)
buildingPanel p Colony{ buildingTask } = do
  case buildingTask of
    Just BuildingTask{ minedMineral } ->
      Widget.label p (fromString $ printf "Building: Mine for Mineral #%d" minedMineral)
    Nothing ->
      Widget.label p "Building: nothing"
  Widget.button (Rect (p + V2 0 20) (V2 256 20)) "Build mine for Mineral #0"
    <&> whenAlt BuildMine

shipBuildingPanel :: V2 Int -> Colony -> Updating (Maybe Action)
shipBuildingPanel p Colony{ shipBuildingTask } = do
  case shipBuildingTask of
    Just ShipBuildingTask{} -> Widget.label p "Producing: Ship"
    Nothing -> Widget.label p "Producing: nothing"
  Widget.button (Rect (p + V2 0 20) (V2 256 20)) "Produce ship"
    <&> whenAlt BuildShip
