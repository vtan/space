module Main where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.GameState as GameState
import qualified App.Render as Render
import qualified App.Render.RenderContext as RenderContext
import qualified App.Render.Rendering as Rendering
import qualified App.Render.RenderState as RenderState
import qualified App.Update as Update
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Font as SDL.TTF

import SDL (($=))

main :: IO ()
main = do
  SDL.initializeAll
  SDL.TTF.initialize
  window <- SDL.createWindow "" 
    $ SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 12
  let renderContext = RenderContext.new renderer font

  fcInitial <- FpsCounter.new
  flip fix (fcInitial, GameState.initial, RenderState.initial) $ \cont (fpsCounter, gs, rs) -> do
    start <- SDL.Raw.getPerformanceCounter
    case fpsCounter ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    let !gs' = gs & Update.update (fpsCounter ^. #lastFrameTime) events

    ((), rs') <- Rendering.runFrame renderContext rs (Render.render gs')

    end <- SDL.Raw.getPerformanceCounter
    if gs' ^. #quit
    then pure ()
    else do
      fc' <- FpsCounter.record fpsCounter (end - start)
      cont (fc', gs', rs')
  SDL.TTF.quit
  SDL.quit
