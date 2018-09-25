module Main (main) where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.Model.GameState as GameState
import qualified App.Render.Rendering as Rendering
import qualified App.Update as Update
import qualified App.Update.UILayout as Update.UILayout
import qualified App.Update.Updating as Updating
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import Control.Exception (SomeException, displayException, try)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  try main' >>= \case
    Right () -> pure ()
    Left (ex :: SomeException) -> do
      let message = displayException ex
      hPutStrLn stderr message
      SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)

main' :: IO ()
main' = do
  SDL.initializeAll
  SDL.TTF.initialize
  window <- SDL.createWindow ""
    $ SDL.defaultWindow { SDL.windowInitialSize = V2 1728 972 }
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 17
  uiLayout <- loadUILayout "data/layout.json"
  let renderContext = Rendering.newContext renderer font
      updateContext = Updating.Context uiLayout
  SDL.Raw.startTextInput

  fcInitial <- FpsCounter.new
  flip fix (fcInitial, GameState.initial, Updating.initialState, Rendering.initialState) $ \cont (fc, gs, us, rs) -> do
    case fc ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    SDL.P (fmap fromIntegral -> mousePos) <- SDL.getAbsoluteMouseLocation
    let (!gs', !us') = Update.update gs
          & Updating.runFrame (fc ^. #lastFrameTime) mousePos events updateContext us
        Updating.State{ Updating.deferredRendering } = us'
        flatRendering = foldl' (*>) (pure ()) deferredRendering
    ((), rs') <- flatRendering & Rendering.runFrame renderContext rs

    if us' ^. #quit
    then pure ()
    else do
      fc' <- FpsCounter.record fc
      cont (fc', gs', us', rs')
  SDL.TTF.quit
  SDL.quit

loadUILayout :: FilePath -> IO Update.UILayout.UILayout
loadUILayout path = do
  contents <- ByteString.readFile path
  case Aeson.eitherDecodeStrict' contents of
    Right rootLayout -> pure (Update.UILayout.fromReadRoot rootLayout)
    Left err -> fail err
