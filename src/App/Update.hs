module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Model.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.Widget as Widget
import qualified App.Update.Updating as Updating
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Model.Ship (Ship(..))
import App.Rect (Rect(..))
import App.Update.Events
import App.Update.Updating (Updating)
import App.Util (clamp, showDate, showDuration)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs = do
  hasFocusedWidget <- use #focusedWidget <&> has _Just
  toggleShips <- Updating.consumeEvents (\case 
      KeyPressEvent SDL.ScancodeS | not hasFocusedWidget -> Just ()
      _ -> Nothing
    ) <&> (not . null)
  when toggleShips $ #ui . #shipWindowOpen %= not

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
      let zoomLevel = sqrt $ gs ^. #camera . #scale . _x
          zoomLevel' = clamp 1.5 (zoomLevel + 0.5 * amount) 30
          auInPixels' = zoomLevel' * zoomLevel'
          scale' = V2 auInPixels' (- auInPixels')
      in gs & #camera . #scale .~ scale'
  KeyPressEvent SDL.ScancodePeriod | gs ^. #timeStepPerFrame & has _Nothing -> 
    let now = gs ^. #time
        nextMidnight = (quot (gs ^. #time) 86400 + 1) * 86400
    in gs & Logic.stepTime (nextMidnight - now)
  KeyPressEvent SDL.ScancodeGrave -> gs & #timeStepPerFrame .~ Nothing
  KeyPressEvent SDL.Scancode1 -> gs & #timeStepPerFrame .~ Just 1
  KeyPressEvent SDL.Scancode2 -> gs & #timeStepPerFrame .~ Just 10
  KeyPressEvent SDL.Scancode3 -> gs & #timeStepPerFrame .~ Just 100
  KeyPressEvent SDL.Scancode4 -> gs & #timeStepPerFrame .~ Just 1200
  KeyPressEvent SDL.Scancode5 -> gs & #timeStepPerFrame .~ Just (3 * 3600)
  _ -> gs

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #shipWindowOpen) >>= \case
    True -> do
      Widget.window (Rect (V2 32 32) (V2 (128 + 256 + 16) (256 + 24))) 16 "Ships"
      let p = V2 36 52

      (selectedShip, clickedShip) <- use (#ui . #selectedShipUid) >>= Widget.listBox
        (Rect p (V2 128 256)) 16
        (view #uid) (view #name)
        (gs ^.. #ships . folded)
      case clickedShip of
        Just Ship{ Ship.uid, Ship.name } -> do
          #ui . #selectedShipUid .= Just uid
          #ui . #editedShipName .= Just name
        Nothing -> pure ()

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
              | moveTo = Logic.moveShipToBody <$> pure ship <*> selectedBody <*> pure gs
              | cancel = Just (Logic.cancelShipOrder ship gs)
              | rename = Just (gs & #ships . at uid . _Just . #name .~ ename')
              | otherwise = Nothing
        pure $ gs' & fromMaybe gs

      let gs' = gsMay' & fromMaybe gs
      pure gs'
    False -> pure gs