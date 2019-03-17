module App.Update.FrameController
  ( update )
where

import App.Prelude

import qualified App.Logic.TimeStep as Logic.TimeStep
import qualified App.UIBuilder.UIBuilder as UIBuilder
import qualified App.Update.ColonyWindow as ColonyWindow
import qualified App.Update.ScreenOverlay as ScreenOverlay
import qualified App.Update.SystemMap as SystemMap
import qualified App.Update.UIState as UIState
import qualified SDL

import App.Common.EventPatterns
import App.Model.GameState (GameState(..))
import App.Update.Updating (Updating)

update :: GameState -> Updating GameState
update gs = do
  Any reloadResources <- view (#uiBuilderContext . #keyModifier) >>= \case
    (SDL.keyModifierLeftCtrl -> True) ->
      UIBuilder.consumeEvents $ \case
        KeyPressEvent SDL.ScancodeR -> Any True
        _ -> mempty
    _ -> pure mempty
  #reloadResources .= reloadResources

  clickedAnywhere <- use (#uiBuilderState . #events)
    <&> find (\case MousePressEvent _ _ -> True; _ -> False)
    <&> (not . null)
  when clickedAnywhere $
    #uiBuilderState . #focusedWidgetName .= Nothing -- if clicked on a focusable widget, it will consume the click and set the focus

  gs' <- gs & (
      fmap UIBuilder.group' handleUI
      >=> fmap UIBuilder.group' ScreenOverlay.update
      >=> SystemMap.update
    )

  timeStep <- use #timeStepPerFrame
  pure $ case timeStep of
    Just step -> gs' & Logic.TimeStep.stepTime step
    Nothing -> gs'

handleUI :: GameState -> Updating GameState
handleUI gs =
  use (#ui . #activeWindow) >>= \case
    Just UIState.ColonyWindow -> ColonyWindow.update gs
    Nothing -> pure gs
