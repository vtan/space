module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Model.Body as Body
import qualified App.Model.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.Widget as Widget
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified SDL

import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.BodyMinerals (MineralData(..))
import App.Model.Ship (Ship(..))
import App.Rect (Rect(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (clamp, showDate, showDuration)
import Data.String (fromString)
import Numeric.Extras (cbrt)

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
  gs'' <- use #events <&> foldl' handleEvent gs'

  pure $ case gs ^. #timeStepPerFrame of
    Just step -> gs'' & Logic.stepTime step
    Nothing -> gs''

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  MousePressEvent SDL.ButtonLeft _ ->
    gs & #movingViewport .~ True
  MousePressEvent SDL.ButtonRight (fmap fromIntegral -> posPx) ->
    let pos = posPx & Camera.screenToPoint (gs ^. #camera)
    in gs & Logic.addShip pos
  MouseReleaseEvent _ ->
    gs & #movingViewport .~ False
  MouseMotionEvent (fmap fromIntegral -> motionPx) ->
    if gs ^. #movingViewport
    then 
      let motionAu = Camera.screenToVector (gs ^. #camera) motionPx
      in gs & #camera . #eyeFrom -~ motionAu
    else gs
  MouseWheelEvent (fromIntegral -> amount) ->
    if gs ^. #movingViewport
    then gs
    else
      let zoomLevel = cbrt $ gs ^. #camera . #scale . _x
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 70
          auInPixels' = zoomLevel' * zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
      in gs & #camera . #scale .~ scale'
  KeyPressEvent SDL.ScancodePeriod | gs ^. #timeStepPerFrame & has _Nothing -> 
    let now = gs ^. #time
    in gs & Logic.stepTime (Logic.timeUntilNextMidnight now)
  KeyPressEvent SDL.ScancodeGrave -> gs & #timeStepPerFrame .~ Nothing
  KeyPressEvent SDL.Scancode1 -> gs & #timeStepPerFrame .~ Just 1
  KeyPressEvent SDL.Scancode2 -> gs & #timeStepPerFrame .~ Just 10
  KeyPressEvent SDL.Scancode3 -> gs & #timeStepPerFrame .~ Just 100
  KeyPressEvent SDL.Scancode4 -> gs & #timeStepPerFrame .~ Just 1200
  KeyPressEvent SDL.Scancode5 -> gs & #timeStepPerFrame .~ Just (3 * 3600)
  _ -> gs

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> handleColonyWindow gs
    Just UIState.ShipWindow -> handleShipWindow gs
    Nothing -> pure gs

handleColonyWindow :: GameState -> Updating GameState
handleColonyWindow gs = do
  Widget.window (Rect (V2 32 32) (V2 (128 + 256 + 16) (256 + 24))) 16 "Colonies"
  let p = V2 36 52

  (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
    (Rect p (V2 128 256)) 16
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
    Widget.labels (p + V2 (128 + 4) 0) 16 mineralLabels
    Widget.labels (p + V2 (128 + 4 + 64) 0) 16 availableLabels
    Widget.labels (p + V2 (128 + 4 + 128) 0) 16 accessibilityLabels

    let q = p + V2 (128 + 4) (length minerals * 16 + 8)
    case gs ^. #colonies . at uid of
      Just Colony{ stockpile, mines } -> do
        Widget.label q "Colony stockpile"
        let (itemLabels, qtyLabels) = unzip $ itoList stockpile <&> \(mineral, qty) ->
              ( fromString $ printf "Mineral #%d" mineral
              , fromString $ printf "%.2f t" qty
              )
        Widget.labels (q + V2 0 16) 16 itemLabels
        Widget.labels (q + V2 64 16) 16 qtyLabels

        let (mineLabels, mineQtyLabels) = unzip $ itoList mines <&> \(mineral, mineQty) ->
              ( fromString $ printf "Mineral #%d" mineral
              , fromString $ printf "%d mines" mineQty
              )
            r = q + V2 0 (length stockpile * 16 + 24)
        Widget.label r "Colony industry"
        Widget.labels (r + V2 0 16) 16 mineLabels
        Widget.labels (r + V2 64 16) 16 mineQtyLabels

        pure gs
      Nothing -> do
        found <- Widget.button (Rect q (V2 128 16)) "Found colony"
        pure $ if found
          then Logic.foundColony uid gs
          else gs
  pure $ fromMaybe gs gsMay'

handleShipWindow :: GameState -> Updating GameState
handleShipWindow gs = do
  Widget.window (Rect (V2 32 32) (V2 (128 + 256 + 16) (256 + 24))) 16 "Ships"
  let p = V2 36 52

  (selectedShip, clickedShip) <- use (#ui . #selectedShipUid) >>= Widget.listBox
    (Rect p (V2 128 256)) 16
    (view #uid) (view #name)
    (gs ^.. #ships . folded)
  for_ clickedShip $ \Ship{ Ship.uid, Ship.name } -> do
      #ui . #selectedShipUid .= Just uid
      #ui . #editedShipName .= Just name

  gsMay' <- for selectedShip $ \ship@Ship{ Ship.uid, Ship.speed, Ship.order } -> do
    ename <- use (#ui . #editedShipName) <&> fromMaybe "???"
    ename' <- Widget.textBox "shipName" (Rect (p + V2 (128 + 4) 0) (V2 128 12)) ename
    #ui . #editedShipName .= Just ename'
    rename <- Widget.button (Rect (p + V2 (128 + 4 + 128 + 4) 0) (V2 128 12)) "Rename"
    
    let commonLabels = [fromString (printf "Speed: %.0f km/s" (speed * 149597000))] -- TODO magic number
        orderLabels = case order of
          Just o ->
            let (orderStr, etaStr) = case o of
                  Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                    let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                        etaDate = showDate (path ^. #endTime)
                        etaDuration = showDuration (path ^. #endTime - gs ^. #time)
                    in (printf "move to %s" bodyName, printf "%s, %s" etaDate etaDuration)
            in fromString <$> ["Current order: " ++ orderStr, "ETA: " ++ etaStr]
          Nothing -> ["No current order"]
    Widget.labels (p + V2 (128 + 4) 16) 16 (commonLabels ++ orderLabels)

    moveTo <- Widget.button (Rect (p + V2 (128 + 4) 64) (V2 56 12)) "Move to..."
    cancel <- Widget.button (Rect (p + V2 (128 + 4 + 56 + 4) 64) (V2 56 12)) "Cancel"

    (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
      (Rect (p + V2 (128 + 4) 80) (V2 128 176)) 16
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