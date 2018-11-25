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
      capability ^? _Ctor @"Freighter"
    availableOnColony <- gs ^? #colonies . at bodyId . _Just . #stockpile . at resource . _Just
    let remainingCargoSpace = cargoCapacity - sum loadedCargo
    let loadedQty = minimum (toList qtyOrAll ++ [availableOnColony, remainingCargoSpace])
    pure $ gs
      & #ships . at shipId . _Just . #capability . _Ctor @"Freighter"
        . #loadedCargo . at resource . non 0 +~ loadedQty
      & #colonies . at bodyId . _Just . #stockpile . at resource . non 0 -~ loadedQty

unloadResourceFromShip :: Maybe Double -> Resource -> Ship -> GameState -> GameState
unloadResourceFromShip qtyOrAll resource Ship{ shipId, attachedToBody, capability } gs =
  fromMaybe gs $ do
    bodyId <- attachedToBody
    Ship.FreighterCapability{ loadedCargo } <- capability ^? _Ctor @"Freighter"
    _ <- gs ^. #colonies . at bodyId
    availableOnShip <- loadedCargo ^. at resource
    let unloadedQty = minimum (toList qtyOrAll ++ [availableOnShip])
    pure $ gs
      & #ships . at shipId . _Just . #capability . _Ctor @"Freighter"
        . #loadedCargo . at resource . non 0 -~ unloadedQty
      & #colonies . at bodyId . _Just . #stockpile . at resource . non 0 +~ unloadedQty

loadPopulationToShip :: Maybe Int -> Ship -> GameState -> GameState
loadPopulationToShip qtyOrAll Ship{ shipId, attachedToBody, capability } gs =
  fromMaybe gs $ do
    bodyId <- attachedToBody
    Ship.ColonyShipCapability{ cabinCapacity, loadedPopulation } <-
      capability ^? _Ctor @"ColonyShip"
    populationOnColony <- gs ^? #colonies . at bodyId . _Just . #population
    let remainingCabinSpace = cabinCapacity - loadedPopulation
    let loadedQty = minimum (toList qtyOrAll ++ [populationOnColony, remainingCabinSpace])
    pure $ gs
      & #ships . at shipId . _Just . #capability . _Ctor @"ColonyShip" . #loadedPopulation +~ loadedQty
      & #colonies . at bodyId . _Just . #population -~ loadedQty

unloadPopulationFromShip :: Maybe Int -> Ship -> GameState -> GameState
unloadPopulationFromShip qtyOrAll Ship{ shipId, attachedToBody, capability } gs =
  fromMaybe gs $ do
    bodyId <- attachedToBody
    Ship.ColonyShipCapability{ loadedPopulation } <-
      capability ^? _Ctor @"ColonyShip"
    _ <- gs ^. #colonies . at bodyId
    let unloadedQty = minimum (toList qtyOrAll ++ [loadedPopulation])
    pure $ gs
      & #ships . at shipId . _Just . #capability . _Ctor @"ColonyShip" . #loadedPopulation -~ unloadedQty
      & #colonies . at bodyId . _Just . #population +~ unloadedQty
