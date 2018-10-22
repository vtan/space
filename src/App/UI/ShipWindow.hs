module App.UI.ShipWindow
  ( update )
where

import App.Prelude

import qualified App.Dimension.Speed as Speed
import qualified App.Dimension.Time as Time
import qualified App.Logic.Ship as Logic.Ship
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text
import qualified Linear as Lin

import App.Common.Uid (Uid)
import App.Common.Util (whenAlt)
import App.Model.Body (Body)
import App.Model.GameState (GameState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))
import App.Update.Updating (Updating)
import Data.String (fromString)
import Text.Read (readMaybe)

data Action
  = Rename Text
  | MoveToBody (Uid Body)
  | CancelOrder
  | LoadResource Resource (Maybe Double)
  | UnloadResource Resource (Maybe Double)
  | LoadPopulation (Maybe Int)
  | UnloadPopulation (Maybe Int)

update :: GameState -> Updating GameState
update gs =
  Updating.useWidget "shipWindow" $ do
    Updating.widget "decoration" $
      Widget.window 20 "Ships"

    (selectedShip, clickedShip) <-
      Updating.widget "selectedShip" $
        Widget.listBox
          20 (view #uid) (view #name)
          #selectedShip
          (gs ^.. #ships . folded)
    for_ clickedShip $ \ship -> #ui . #editedShipName .= (ship ^. #name)

    gs' <- Updating.useWidget "rightPanel" $
      for selectedShip $ \ship@Ship{ uid } -> do
        rename <- Updating.useWidget "renamePanel" renamePanel
        infoLabels ship gs

        moveTo <- Updating.widget "moveTo"
          (Widget.button "Move to...")
          <&> whenAlt ()
        cancel <- Updating.widget "cancel"
          (Widget.button "Cancel")
          <&> whenAlt CancelOrder

        selectedBody <- Updating.widget "selectedBody" $
          Widget.closedDropdown
            20 380
            (view #uid) (view #name)
            #selectedBody
            (gs ^.. #bodies . folded)

        cargoAction <- Updating.useWidget "cargoPanel" (cargoPanel ship)
        cabinAction <- Updating.useWidget "cabinPanel" (cabinPanel ship)

        let moveToSelected = (moveTo *> selectedBody) <&> \b -> MoveToBody (b ^. #uid)
            action = rename <|> moveToSelected <|> cancel <|> cargoAction <|> cabinAction
        pure $ case action of
          Just (Rename name) ->
            gs & #ships . at uid . _Just . #name .~ name
          Just (MoveToBody bodyUid) ->
            Logic.Ship.moveShipToBody ship bodyUid gs
          Just CancelOrder ->
            Logic.Ship.cancelShipOrder ship gs
          Just (LoadResource resource qtyOrAll) ->
            Logic.Ship.loadResourceToShip qtyOrAll resource ship gs
          Just (UnloadResource resource qtyOrAll) ->
            Logic.Ship.unloadResourceFromShip qtyOrAll resource ship gs
          Just (LoadPopulation qtyOrAll) ->
            Logic.Ship.loadPopulationToShip qtyOrAll ship gs
          Just (UnloadPopulation qtyOrAll) ->
            Logic.Ship.unloadPopulationFromShip qtyOrAll ship gs
          Nothing -> gs

    pure (gs' & fromMaybe gs)

renamePanel :: Updating (Maybe Action)
renamePanel = do
  ename <- Updating.widget "shipName" $
    Widget.textBox "shipName" #editedShipName
  Updating.widget "button"
    (Widget.button "Rename")
    <&> whenAlt (Rename ename)

infoLabels :: Ship -> GameState -> Updating ()
infoLabels Ship{ speed, order } gs = do
  let commonLabels = [fromString (printf "Speed: %s" (Speed.printKmPerSec speed))]
      orderLabels = case order of
        Just o ->
          let (orderStr, etaStr) = case o of
                Ship.MoveToBody{ Ship.bodyUid, Ship.path } ->
                  let bodyName = gs ^? #bodies . at bodyUid . _Just . #name & fromMaybe "???"
                      etaDate = Time.printDate (path ^. #endTime)
                      etaDuration = Time.printDuration (path ^. #endTime - gs ^. #time)
                      actualSpeed = Speed.div
                        (Lin.distance (path ^. #endPos) (path ^. #startPos))
                        (path ^. #endTime - path ^. #startTime)
                  in (printf "move to %s (act. spd. %s)" bodyName (Speed.printKmPerSec actualSpeed), printf "%s, %s" etaDate etaDuration)
          in fromString <$> ["Current order: " ++ orderStr, "ETA: " ++ etaStr]
        Nothing -> ["No current order"]
  Updating.widget "infoLabels" $
    Widget.labels 20 (commonLabels ++ orderLabels)

cargoPanel :: Ship -> Updating (Maybe Action)
cargoPanel Ship{ cargoCapacity, loadedCargo } = do
  selectedResource <- Updating.widget "selectedResource" $
    Widget.closedDropdown
      20 380
      id (show >>> fromString)
      #selectedResource
      Resource.all

  Updating.widget "qtyLabel" $
    Widget.label "Qty:"

  qty <- Updating.widget "qty"
    (Widget.textBox "cargoQty" #editedResourceQty)
    <&> (Text.unpack >>> readMaybe @Double >>> mfilter (>= 0))

  load <- Updating.widget "load"
    (Widget.button "Load")
    <&> whenAlt (LoadResource <$> selectedResource <*> (fmap Just qty))
    <&> join

  loadAll <- Updating.widget "loadAll"
    (Widget.button "Load all")
    <&> whenAlt (LoadResource <$> selectedResource <*> Just Nothing)
    <&> join

  unload <- Updating.widget "unload"
    (Widget.button "Unload")
    <&> whenAlt (UnloadResource <$> selectedResource <*> (fmap Just qty))
    <&> join

  unloadAll <- Updating.widget "unloadAll"
    (Widget.button "Unload all")
    <&> whenAlt (UnloadResource <$> selectedResource <*> Just Nothing)
    <&> join

  Updating.widget "cargoCapacity" $
    Widget.label $
      fromString (printf "Cargo capacity: %.0f t" cargoCapacity)

  Updating.widget "loadedCargo" $
    Widget.labels 20 $
      loadedCargo
        & itoList
        & map (\(resource, loadedQty) ->
          fromString (printf "Stored %s: %.0f t" (show resource) loadedQty)
        )

  pure $ load <|> loadAll <|> unload <|> unloadAll

cabinPanel :: Ship -> Updating (Maybe Action)
cabinPanel Ship{ cabinCapacity, loadedPopulation } = do
  Updating.widget "cabinCapacity" $
    Widget.label $
      fromString (printf "Cabin capacity: %d / %d ppl" loadedPopulation cabinCapacity)

  Updating.widget "popLabel" $
    Widget.label "Pop:"

  qty <- Updating.widget "pop"
    (Widget.textBox "popQty" #editedPopulationQty)
    <&> (Text.unpack >>> readMaybe @Int >>> mfilter (>= 0))

  let (loadLabel, unloadLabel) = case qty of
        Just _ -> ("Load", "Unload")
        Nothing -> ("Load max", "Unload max")

  load <- Updating.widget "load"
    (Widget.button loadLabel)
    <&> whenAlt (LoadPopulation qty)

  unload <- Updating.widget "unload"
    (Widget.button unloadLabel)
    <&> whenAlt (UnloadPopulation qty)

  let action = load <|> unload
  when (action & has _Just) $
    #ui . #editedPopulationQty .= ""

  pure action
