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
  QuitEvent -> gs & #quit .~ True
  _ -> gs