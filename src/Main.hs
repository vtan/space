module Main where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.GameState as GameState
import qualified App.Render as Render
import qualified App.Update as Update
import qualified Data.Text as Text
import qualified SDL
import qualified SDL.Raw

import SDL (($=))
import Text.Printf (printf)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  fcInitial <- FpsCounter.new
  flip fix (fcInitial, GameState.initial) $ \cont (fpsCounter, gs) -> do
    start <- SDL.Raw.getPerformanceCounter
    case fpsCounter ^. #updatedFps of
      Just updated -> SDL.windowTitle window $= Text.pack (printf "FPS: %.2f" updated)
      Nothing -> pure ()

    events <- SDL.pollEvents
    let !gs' = gs & Update.update (fpsCounter ^. #lastFrameTime) events

    Render.render renderer gs'

    end <- SDL.Raw.getPerformanceCounter
    if gs' ^. #quit
    then pure ()
    else cont (FpsCounter.record fpsCounter (end - start), gs')
  SDL.quit
