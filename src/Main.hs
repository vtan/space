module Main (main) where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.Render.Rendering as Rendering
import qualified App.Update as Update
import qualified App.Update.Initial as Initial
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget2 as Widget2
import qualified App.Update.WidgetTree as Update.WidgetTree
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import App.Model.GameState (GameState)
import Control.Exception (Exception, SomeException, displayException, handle)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

data MainContext = MainContext
  { window :: SDL.Window
  , screenSize :: V2 Int
  , renderContext :: Rendering.Context
  }

data MainState = MainState
  { fpsCounter :: FpsCounter.Counter
  , resourceContext :: Updating.ResourceContext
  , gameState :: GameState
  , updateState :: Updating.State
  , renderState :: Rendering.State
  }

main :: IO ()
main =
  handle (\(ex :: SomeException) -> logError ex) $ do
    SDL.initializeAll
    SDL.TTF.initialize
    let screenSize = V2 1728 972
    window <- SDL.createWindow "" SDL.defaultWindow{ SDL.windowInitialSize = screenSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{ SDL.rendererType = SDL.AcceleratedVSyncRenderer }
    font <- SDL.TTF.load "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf" 17
    let renderContext = Rendering.newContext renderer font
    resourceContext <- loadResourceContext
    SDL.Raw.startTextInput

    fpsCounter <- FpsCounter.new
    let gameState = Initial.gameState
        updateState = Updating.initialState
        renderState = Rendering.initialState
    mainLoop
      MainContext{ window, screenSize, renderContext }
      MainState{ fpsCounter, resourceContext, gameState, updateState, renderState }

    SDL.TTF.quit
    SDL.quit

mainLoop :: MainContext -> MainState -> IO ()
mainLoop
    ctx@MainContext{ window, screenSize, renderContext }
    MainState{ fpsCounter, resourceContext, gameState, updateState, renderState } =
  do
    case fpsCounter ^. #updatedText of
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
    let uc = Updating.contextFrom resourceContext frc
        (!gameState', !updateState') = Update.update gameState
          & Updating.runFrame events uc updateState
        Updating.State{ Updating.deferredRendering, Updating.ui2 = Widget2.UIState{ renderStack } } = updateState'
        flatRendering = foldl' (*>) (pure ()) (deferredRendering ++ toList renderStack)
    ((), renderState') <- flatRendering & Rendering.runFrame renderContext renderState

    resourceContext' <-
      if updateState' ^. #reloadResources
      then reloadResourceContext <&> fromMaybe resourceContext
      else pure resourceContext

    if updateState' ^. #quit
    then pure ()
    else do
      fpsCounter' <- FpsCounter.record fpsCounter
      mainLoop ctx (MainState fpsCounter' resourceContext' gameState' updateState' renderState')

loadResourceContext :: IO Updating.ResourceContext
loadResourceContext = do
  contents <- ByteString.readFile "data/layout.json"
  case Aeson.eitherDecodeStrict' contents of
    Right widgetLayout ->
      pure Updating.ResourceContext{ widgetTree = Update.WidgetTree.fromWidgetLayout widgetLayout }
    Left err -> fail err

reloadResourceContext :: IO (Maybe Updating.ResourceContext)
reloadResourceContext =
  handle
    (\(ex :: SomeException) -> Nothing <$ logError ex)
    (Just <$> loadResourceContext)

logError :: Exception e => e -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
