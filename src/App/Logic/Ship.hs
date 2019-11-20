module App.Logic.Ship where

import App.Prelude

import qualified App.Model.PlottedPath as PlottedPath
import qualified App.Model.Ship as Ship

import App.Common.Id (Id(..))
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import App.Model.Resource (Resource)
import App.Model.Ship (Ship(..))

moveShipToBody :: Ship -> Id Body -> GameState -> GameState
moveShipToBody Ship{ shipId, Ship.position, speed } bodyId gs =
  let pathMay = PlottedPath.plot (gs ^. #time) position speed bodyId (gs ^. #rootBody)
      orderMay = Ship.MoveToBody <$> pure bodyId <*> pathMay
  in gs & #ships . at shipId . _Just . #order .~ orderMay

cancelShipOrder :: Ship -> GameState -> GameState
cancelShipOrder Ship{ shipId } gs =
  gs & #ships . at shipId . _Just . #order .~ Nothing

loadResourceToShip :: Maybe Double -> Resource -> Ship -> GameState -> GameState
loadResourceToShip qtyOrAll resource Ship{ shipId, attachedToBody, capability } gs =
  fromMaybe gs $ do
    bodyId <- attachedToBody
    Ship.FreighterCapability{ cargoCapacity, loadedCargo } <-
      capability ^? #_Freighter
    availableOnColony <- gs ^? #colonies . at bodyId . _Just . #stockpile . at resource . _Just
    let remainingCargoSpace = cargoCapacity - sum loadedCargo
    let loadedQty = minimum (toList qtyOrAll ++ [availableOnColony, remainingCargoSpace])
    pure $ gs
      & #ships . at shipId . _Just . #capability . #_Freighter
        . #loadedCargo . at resource . non 0 +~ loadedQty
      & #colonies . at bodyId . _Just . #stockpile . at resource . non 0 -~ loadedQty

unloadResourceFromShip :: Maybe Double -> Resource -> Ship -> GameState -> GameState
unloadResourceFromShip qtyOrAll resource Ship{ shipId, attachedToBody, capability } gs =
  fromMaybe gs $ do
    bodyId <- attachedToBody
    Ship.FreighterCapability{ loadedCargo } <- capability ^? #_Freighter
    _ <- gs ^. #colonies . at bodyId
    availableOnShip <- loadedCargo ^. at resource
    let unloadedQty = minimum (toList qtyOrAll ++ [availableOnShip])
    pure $ gs
      & #ships . at shipId . _Just . #capability . #_Freighter
        . #loadedCargo . at resource . non 0 -~ unloadedQty
      & #colonies . at bodyId . _Just . #stockpile . at resource . non 0 +~ unloadedQty
