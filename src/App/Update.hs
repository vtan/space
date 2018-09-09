module App.Update
  ( update )
where

import App.Prelude

import qualified App.Model.Body as Body
import qualified App.Model.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.SystemMap as SystemMap
import qualified App.Update.Widget as Widget
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified Linear as Lin
import qualified SDL

import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.BodyMinerals (MineralData(..))
import App.Model.Ship (Ship(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Rect (Rect(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (showDate, showDuration)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs = do
  hasFocusedWidget <- use #focusedWidget <&> has _Just
  toggleWindow <- Updating.consumeEvents (\case
      KeyPressEvent SDL.ScancodeC | not hasFocusedWidget -> Just UIState.ColonyWindow
      KeyPressEvent SDL.ScancodeS | not hasFocusedWidget -> Just UIState.ShipWindow
      _ -> Nothing
    ) <&> listToMaybe
  case toggleWindow of
    Just window -> do
      activeWindow <- use (#ui . #activeWindow)
      if elem window activeWindow
      then #ui . #activeWindow .= Nothing
      else #ui . #activeWindow .= Just window
    Nothing -> pure ()

  clickedAnywhere <- Updating.filterEvents (\case MousePressEvent _ _ -> Just (); _ -> Nothing)
    <&> (not . null)
  when clickedAnywhere $ #focusedWidget .= Nothing -- if clicked on a focusable widget, it will consume the click and set the focus

  gs' <- handleUI gs
  gs'' <- SystemMap.update gs'

  pure $ case gs ^. #timeStepPerFrame of
    Just step -> gs'' & Logic.stepTime step
    Nothing -> gs''

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> handleColonyWindow gs
    Just UIState.ShipWindow -> handleShipWindow gs
    Nothing -> pure gs

handleColonyWindow :: GameState -> Updating GameState
handleColonyWindow gs = do
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

handleShipWindow :: GameState -> Updating GameState
handleShipWindow gs = do
  Widget.window (Rect (V2 32 32) (V2 (4 + 600 + 5) (500 + 24))) 20 "Ships"
  let p = V2 36 56

  (selectedShip, clickedShip) <- use (#ui . #selectedShipUid) >>= Widget.listBox
    (Rect p (V2 200 500)) 20
    (view #uid) (view #name)
    (gs ^.. #ships . folded)
  for_ clickedShip $ \Ship{ Ship.uid, Ship.name } -> do
      #ui . #selectedShipUid .= Just uid
      #ui . #editedShipName .= Just name

  gsMay' <- for selectedShip $ \ship@Ship{ Ship.uid, Ship.speed, Ship.order } -> do
    ename <- use (#ui . #editedShipName) <&> fromMaybe "???"
    ename' <- Widget.textBox "shipName" (Rect (p + V2 (200 + 4) 0) (V2 160 20)) ename
    #ui . #editedShipName .= Just ename'
    rename <- Widget.button (Rect (p + V2 (200 + 4 + 160 + 4) 0) (V2 100 20)) "Rename"

    let commonLabels = [fromString (printf "Speed: %.0f km/s" (speed * 149597000))] -- TODO magic number
        orderLabels = case order of
          Just o ->
            let (orderStr, etaStr) = case o of
                  Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                    let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                        etaDate = showDate (path ^. #endTime)
                        etaDuration = showDuration (path ^. #endTime - gs ^. #time)
                        actualSpeed = Lin.distance (path ^. #endPos) (path ^. #startPos) / fromIntegral (path ^. #endTime - path ^. #startTime) * 149597000
                    in (printf "move to %s (act. spd. %.2f)" bodyName actualSpeed, printf "%s, %s" etaDate etaDuration)
            in fromString <$> ["Current order: " ++ orderStr, "ETA: " ++ etaStr]
          Nothing -> ["No current order"]
    Widget.labels (p + V2 (200 + 4) 28) 20 (commonLabels ++ orderLabels)

    moveTo <- Widget.button (Rect (p + V2 (200 + 4) 92) (V2 80 20)) "Move to..."
    cancel <- Widget.button (Rect (p + V2 (200 + 4 + 80 + 4) 92) (V2 80 20)) "Cancel"

    (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
      (Rect (p + V2 (200 + 4) 120) (V2 200 380)) 20
      (view #uid) (view #name)
      (gs ^.. #bodies . folded)
    when (clickedBody & has _Just) $
      #ui . #selectedBodyUid .= selectedBody ^? _Just . #uid

    let gs'
          | moveTo = Logic.moveShipToBody <$> pure ship <*> (selectedBody ^? _Just . #uid) <*> pure gs
          | cancel = Just (Logic.cancelShipOrder ship gs)
          | rename = Just (gs & #ships . at uid . _Just . #name .~ ename')
          | otherwise = Nothing
    pure $ gs' & fromMaybe gs

  let gs' = gsMay' & fromMaybe gs
  pure gs'
