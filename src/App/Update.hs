module App.Update 
  ( update )
where

import App.Prelude

import qualified SDL as SDL

import App.GameState (GameState(..))
import App.Update.Events

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #totalTime (+ lastFrameTime)
  >>> (\gs -> foldl' handleEvent gs events)

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs = \case
  QuitEvent -> 
    gs & #quit .~ True
  MousePressEvent (_ :: V2 Int) ->
    gs & #movingViewport .~ True
  MouseReleaseEvent (_ :: V2 Int) ->
    gs & #movingViewport .~ False
  MouseMotionEvent (relMotion :: V2 Int) ->
    if gs ^. #movingViewport
    then gs & #camera . #translate +~ fmap fromIntegral relMotion
    else gs
  _ -> gs