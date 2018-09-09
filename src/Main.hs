module Main (main) where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.Model.GameState as GameState
import qualified App.Render.Rendering as Rendering
import qualified App.Update as Update
import qualified App.Update.Updating as Updating
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

main :: IO ()
main = do
  SDL.initializeAll
  SDL.TTF.initialize
  window <- SDL.createWindow "" 
    $ SDL.defaultWindow { SDL.windowInitialSize = V2 1728 972 }
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 17
  let renderContext = Rendering.newContext renderer font
  SDL.Raw.startTextInput

  fcInitial <- FpsCounter.new
  flip fix (fcInitial, GameState.initial, Updating.initialState, Rendering.initialState) $ \cont (fc, gs, us, rs) -> do
    case fc ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    let (!gs', !us') = Update.update gs
          & Updating.runFrame (fc ^. #lastFrameTime) events us
        Updating.State{ Updating.deferredRendering } = us'
    ((), rs') <- deferredRendering & Rendering.runFrame renderContext rs 

    if us' ^. #quit
    then pure ()
    else do
      fc' <- FpsCounter.record fc
      cont (fc', gs', us', rs')
  SDL.TTF.quit
  SDL.quit
