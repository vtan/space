module Main (main) where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.Render.Rendering as Rendering
import qualified App.Update as Update
import qualified App.Update.Initial as Initial
import qualified App.Update.Updating as Updating
import qualified App.Update.WidgetTree as Update.WidgetTree
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import Control.Exception (Exception, SomeException, displayException, try)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  try main' >>= \case
    Right () -> pure ()
    Left (ex :: SomeException) -> logError ex

main' :: IO ()
main' = do
  SDL.initializeAll
  SDL.TTF.initialize
  window <- SDL.createWindow ""
    $ SDL.defaultWindow { SDL.windowInitialSize = V2 1728 972 }
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 17
  let renderContext = Rendering.newContext renderer font
  updateContext <- loadUpdateContext
  SDL.Raw.startTextInput

  fcInitial <- FpsCounter.new
  flip fix (fcInitial, Initial.gameState, updateContext, Updating.initialState, Rendering.initialState) $ \cont (fc, gs, uc, us, rs) -> do
    case fc ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    keyMod <- SDL.getModState
    SDL.P (fmap fromIntegral -> mousePos) <- SDL.getAbsoluteMouseLocation
    let (!gs', !us') = Update.update gs
          & Updating.runFrame (fc ^. #lastFrameTime) mousePos events keyMod uc us
        Updating.State{ Updating.deferredRendering } = us'
        flatRendering = foldl' (*>) (pure ()) deferredRendering
    ((), rs') <- flatRendering & Rendering.runFrame renderContext rs

    uc' <-
      if us' ^. #reloadResources
      then reloadUpdateContext <&> fromMaybe uc
      else pure uc

    if us' ^. #quit
    then pure ()
    else do
      fc' <- FpsCounter.record fc
      cont (fc', gs', uc', us', rs')
  SDL.TTF.quit
  SDL.quit

loadUpdateContext :: IO Updating.Context
loadUpdateContext = do
  contents <- ByteString.readFile "data/layout.json"
  case Aeson.eitherDecodeStrict' contents of
    Right widgetLayout ->
      pure $ Updating.Context (Update.WidgetTree.fromWidgetLayout widgetLayout)
    Left err -> fail err

reloadUpdateContext :: IO (Maybe Updating.Context)
reloadUpdateContext =
  try loadUpdateContext >>= \case
    Right ctx -> pure (Just ctx)
    Left (ex :: SomeException) -> Nothing <$ logError ex

logError :: Exception e => e -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
