module Main (main) where

import App.Prelude

import qualified App.Common.FpsCounter as FpsCounter
import qualified App.Logic.InitialGameState as InitialGameState
import qualified App.Render.Render as Render
import qualified App.Update.FrameController as FrameController
import qualified App.Update.Update as Update
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import App.Model.GameState (GameState)
import App.Render.Render (RenderContext, RenderState)
import App.Update.Update (UpdateContext(..), UpdateState(..))
import App.UIBuilder.UIBuilder (UIBuilderContext(..), UIBuilderState(..))
import Control.Exception (SomeException, displayException, handle)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

data MainContext = MainContext
  { window :: SDL.Window
  , renderContext :: RenderContext
  , screenSize :: V2 Int
  }

data MainState = MainState
  { fpsCounter :: FpsCounter.Counter
  , scaleFactor :: Int
  , gameState :: GameState
  , updateState :: UpdateState
  , renderState :: RenderState
  }

main :: IO ()
main =
  handle logError $ do
    SDL.initializeAll
    SDL.TTF.initialize
    let screenSize = V2 1728 972
    window <- SDL.createWindow "" SDL.defaultWindow{ SDL.windowInitialSize = screenSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{ SDL.rendererType = SDL.AcceleratedVSyncRenderer }
    renderState <- Render.newState fontPath (fontSize 4)
    let renderContext = Render.newContext renderer
    SDL.Raw.startTextInput

    fpsCounter <- FpsCounter.new
    let gameState = InitialGameState.initial
        updateState = Update.initialState
        scaleFactor = 4
    mainLoop
      MainContext{ window, renderContext, screenSize }
      MainState{ fpsCounter, scaleFactor, gameState, updateState, renderState }

    SDL.TTF.quit
    SDL.quit

mainLoop :: MainContext -> MainState -> IO ()
mainLoop
    ctx@MainContext{ window, renderContext, screenSize }
    MainState{ fpsCounter, scaleFactor, gameState, updateState, renderState } =
  do
    case fpsCounter ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    uc <- do
      keyMod <- SDL.getModState
      SDL.P (fmap fromIntegral -> mousePos) <- SDL.getAbsoluteMouseLocation
      pure UpdateContext
        { uiBuilderContext = UIBuilderContext
          { keyModifier = keyMod
          , mousePosition = mousePos
          , screenSize = screenSize
          , scaleFactor = scaleFactor
          }
        }
    let (!gameState', !updateState') = FrameController.update gameState
          & Update.runFrame events uc updateState
        UpdateState{ Update.uiBuilderState = UIBuilderState{ renderStack } } = updateState'
        flatRender = foldl' (*>) (pure ()) (toList renderStack)
    ((), renderState') <- flatRender & Render.runFrame renderContext renderState

    (scaleFactor', renderState'') <-
      case updateState' ^. #newScaleFactor of
        Just new -> do
          rs <- reloadFont new renderState'
          pure (new, rs)
        Nothing -> pure (scaleFactor, renderState')

    if updateState' ^. #quit
    then pure ()
    else do
      fpsCounter' <- FpsCounter.record fpsCounter
      mainLoop ctx (MainState fpsCounter' scaleFactor' gameState' updateState' renderState'')

fontPath :: String
fontPath = "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf"

fontSize :: Int -> Int
fontSize scaleFactor = 1 + 4 * scaleFactor

reloadFont :: Int -> RenderState -> IO RenderState
reloadFont newFontSize rs =
  handle (\ex -> rs <$ logError ex) $
    Render.reloadFont fontPath (fontSize newFontSize) rs

logError :: SomeException -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
