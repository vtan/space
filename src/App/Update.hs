module App.Update
  ( update )
where

import App.Prelude

import qualified App.Common.Print as Print
import qualified App.Logic.TimeStep as Logic.TimeStep
import qualified App.UI.ColonyWindow as ColonyWindow
import qualified App.UI.ProductionWindow as ProductionWindow
import qualified App.UI.ScreenOverlay as ScreenOverlay
import qualified App.UI.ShipWindow as ShipWindow
import qualified App.UI.SystemMap as SystemMap
import qualified App.Update.UIState as UIState
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget2 as Widget2
import qualified SDL

import App.Model.GameState (GameState(..))
import App.Update.Events
import App.Update.Updating (Updating)

update :: GameState -> Updating GameState
update gs = do
  reloadResources <- view (#frameContext . #keyModifier) >>= \case
    (SDL.keyModifierLeftCtrl -> True) ->
      Updating.consumeEvents (\case
        KeyPressEvent SDL.ScancodeR -> Just ()
        _ -> Nothing
      ) <&> (not . null)
    _ -> pure False
  #reloadResources .= reloadResources

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

  activeDropdown <- use #activeDropdown
  for_ activeDropdown $ \updateDropdown ->
    updateDropdown *> Updating.pushRendering

  gs' <- gs & (
      handleUI
      >=> ScreenOverlay.update
      >=> SystemMap.update
    )

  Widget2.label "label1"
  Widget2.label "label2"
  Widget2.label "label3"
  Widget2.group Widget2.Horizontal $
    Widget2.sized 10 $ do
      Widget2.button "+1" >>= \click -> when click (#ui . #testNumber += 1)
      Widget2.button "*2" >>= \click -> when click (#ui . #testNumber *= 2)
      Widget2.label =<< Print.int <$> use (#ui . #testNumber)
  shipName <- Widget2.textBox "shipName" (#ui . #editedShipName)
  _ <- Widget2.textBox "shipName2" (#ui . #editedShipName)
  Widget2.label' "hello"
  Widget2.label' shipName
  Widget2.group Widget2.Horizontal $ do
    _ <- Widget2.sized 20 (Widget2.button "LARGE")
    Widget2.sized (V2 2 8) $ do
      Widget2.group Widget2.Vertical $ do
        _ <- Widget2.button "1"
        _ <- Widget2.button "2"
        pure ()
      Widget2.group Widget2.Vertical $ do
        _ <- Widget2.button "3"
        _ <- Widget2.button "4"
        pure ()
    Widget2.group Widget2.Vertical $
      Widget2.sized (V2 20 4) $ do
        Widget2.label "aaa"
        Widget2.label "bbb"
        Widget2.label "ccc"
  Widget2.label' "hello"

  timeStep <- use #timeStepPerFrame
  pure $ case timeStep of
    Just step -> gs' & Logic.TimeStep.stepTime step
    Nothing -> gs'

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> ColonyWindow.update gs
    Just UIState.ShipWindow -> ShipWindow.update gs
    Just UIState.ProductionWindow -> ProductionWindow.update gs
    Nothing -> pure gs
