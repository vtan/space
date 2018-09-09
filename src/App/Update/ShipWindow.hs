module App.Update.ShipWindow
  ( update )
where

import App.Prelude

import qualified App.Model.Ship as Ship
import qualified App.Update.Logic as Logic
import qualified App.Update.Widget as Widget
import qualified Linear as Lin

import App.Model.Body (Body)
import App.Model.GameState (GameState(..))
import App.Model.Ship (Ship(..))
import App.Rect (Rect(..))
import App.Uid (Uid)
import App.Update.Updating (Updating)
import App.Util (showDate, showDuration, whenAlt)
import Data.String (fromString)

data Action
  = Rename Text
  | MoveToBody (Uid Body)
  | CancelOrder

update :: GameState -> Updating GameState
update gs = do
  Widget.window (Rect (V2 32 32) (V2 (4 + 600 + 5) (500 + 24))) 20 "Ships"
  let p = V2 36 56

  selectedShip <- shipList p gs

  gs' <- for selectedShip $ \ship@Ship{ uid } -> do
    rename <- renamePanel (p + V2 (200 + 4) 0)
    infoLabels (p + V2 (200 + 4) 28) ship gs

    moveTo <- Widget.button (Rect (p + V2 (200 + 4) 92) (V2 80 20)) "Move to..."
      <&> whenAlt ()
    cancel <- Widget.button (Rect (p + V2 (200 + 4 + 80 + 4) 92) (V2 80 20)) "Cancel"
      <&> whenAlt CancelOrder

    selectedBody <- bodyList (p + V2 (200 + 4) 120) gs

    let moveToSelected = (moveTo *> selectedBody) <&> \b -> MoveToBody (b ^. #uid)
        action = rename <|> moveToSelected <|> cancel
    pure $ case action of
      Just (Rename name) -> gs & #ships . at uid . _Just . #name .~ name
      Just (MoveToBody bodyUid) -> Logic.moveShipToBody ship bodyUid gs
      Just CancelOrder -> Logic.cancelShipOrder ship gs
      Nothing -> gs

  pure (gs' & fromMaybe gs)

shipList :: V2 Int -> GameState -> Updating (Maybe Ship)
shipList p gs = do
  (selectedShip, clickedShip) <- use (#ui . #selectedShipUid) >>= Widget.listBox
    (Rect p (V2 200 500)) 20
    (view #uid) (view #name)
    (gs ^.. #ships . folded)
  for_ clickedShip $ \Ship{ Ship.uid, Ship.name } -> do
      #ui . #selectedShipUid .= Just uid
      #ui . #editedShipName .= Just name
  pure selectedShip

renamePanel :: V2 Int -> Updating (Maybe Action)
renamePanel p = do
  ename <- use (#ui . #editedShipName) <&> fromMaybe "???"
  ename' <- Widget.textBox "shipName" (Rect p (V2 160 20)) ename
  #ui . #editedShipName .= Just ename'
  Widget.button (Rect (p + V2 (160 + 4) 0) (V2 100 20)) "Rename"
    <&> whenAlt (Rename ename')

infoLabels :: V2 Int -> Ship -> GameState -> Updating ()
infoLabels p Ship{ speed, order } gs =
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
  in Widget.labels p 20 (commonLabels ++ orderLabels)

bodyList :: V2 Int -> GameState -> Updating (Maybe Body)
bodyList p gs = do
  (selectedBody, clickedBody) <- use (#ui . #selectedBodyUid) >>= Widget.listBox
    (Rect p (V2 200 380)) 20
    (view #uid) (view #name)
    (gs ^.. #bodies . folded)
  when (clickedBody & has _Just) $
    #ui . #selectedBodyUid .= selectedBody ^? _Just . #uid
  pure selectedBody
