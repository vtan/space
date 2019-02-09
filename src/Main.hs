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
  let screenSize = V2 1728 972
  window <- SDL.createWindow ""
    $ SDL.defaultWindow { SDL.windowInitialSize = screenSize }
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 17
  let renderContext = Rendering.newContext renderer font
  resourceContext <- loadResourceContext
  SDL.Raw.startTextInput

  fcInitial <- FpsCounter.new
  flip fix (fcInitial, Initial.gameState, resourceContext, Updating.initialState, Rendering.initialState) $ \cont (fc, gs, rc, us, rs) -> do
    case fc ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    frc <- do
      keyMod <- SDL.getModState
      SDL.P (fmap fromIntegral -> mousePos) <- SDL.getAbsoluteMouseLocation
      pure Updating.FrameContext
        { keyModifier = keyMod
        , mousePosition = mousePos
        , screenSize = screenSize
        }
    let uc = Updating.contextFrom rc frc
        (!gs', !us') = Update.update gs
          & Updating.runFrame events uc us
        Updating.State{ Updating.deferredRendering } = us'
        flatRendering = foldl' (*>) (pure ()) deferredRendering
    ((), rs') <- flatRendering & Rendering.runFrame renderContext rs

    rc' <-
      if us' ^. #reloadResources
      then reloadResourceContext <&> fromMaybe rc
      else pure rc

    if us' ^. #quit
    then pure ()
    else do
      fc' <- FpsCounter.record fc
      cont (fc', gs', rc', us', rs')
  SDL.TTF.quit
  SDL.quit

loadResourceContext :: IO Updating.ResourceContext
loadResourceContext = do
  contents <- ByteString.readFile "data/layout.json"
  case Aeson.eitherDecodeStrict' contents of
    Right widgetLayout ->
      pure Updating.ResourceContext{ widgetTree = Update.WidgetTree.fromWidgetLayout widgetLayout }
    Left err -> fail err

reloadResourceContext :: IO (Maybe Updating.ResourceContext)
reloadResourceContext =
  try loadResourceContext >>= \case
    Right ctx -> pure (Just ctx)
    Left (ex :: SomeException) -> Nothing <$ logError ex

logError :: Exception e => e -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
