module Main (main) where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.Render.Rendering as Rendering
import qualified App.Update as Update
import qualified App.Update.Initial as Initial
import qualified App.Update.Updating as Updating
import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import App.Model.GameState (GameState)
import App.UIBuilder.UIBuilder (UIBuilderContext(..), UIBuilderState(..))
import Control.Exception (SomeException, displayException, handle)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

data MainContext = MainContext
  { window :: SDL.Window
  , renderContext :: Rendering.Context
  , screenSize :: V2 Int
  }

data MainState = MainState
  { fpsCounter :: FpsCounter.Counter
  , scaleFactor :: Int
  , gameState :: GameState
  , updateState :: Updating.State
  , renderState :: Rendering.State
  }

main :: IO ()
main =
  handle logError $ do
    SDL.initializeAll
    SDL.TTF.initialize
    let screenSize = V2 1728 972
    window <- SDL.createWindow "" SDL.defaultWindow{ SDL.windowInitialSize = screenSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{ SDL.rendererType = SDL.AcceleratedVSyncRenderer }
    renderState <- Rendering.newState fontPath (fontSize 4)
    let renderContext = Rendering.newContext renderer
    SDL.Raw.startTextInput

    fpsCounter <- FpsCounter.new
    let gameState = Initial.gameState
        updateState = Updating.initialState
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
      pure Updating.Context
        { uiBuilderContext = UIBuilderContext
          { keyModifier = keyMod
          , mousePosition = mousePos
          , screenSize = screenSize
          , scaleFactor = scaleFactor
          }
        }
    let (!gameState', !updateState') = Update.update gameState
          & Updating.runFrame events uc updateState
        Updating.State{ Updating.uiBuilderState = UIBuilderState{ renderStack } } = updateState'
        flatRendering = foldl' (*>) (pure ()) (toList renderStack)
    ((), renderState') <- flatRendering & Rendering.runFrame renderContext renderState

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

reloadFont :: Int -> Rendering.State -> IO Rendering.State
reloadFont newFontSize rs =
  handle (\ex -> rs <$ logError ex) $
    Rendering.reloadFont fontPath (fontSize newFontSize) rs

logError :: SomeException -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
