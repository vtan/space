module Main (main) where

import App.Prelude

import qualified App.Common.FpsCounter as FpsCounter
import qualified App.Logic.InitialGameState as InitialGameState
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Game.Controller as Controller
import qualified Game.UIState as UIState

import App.Common.EventPatterns
import Core.CoreContext (CoreContext(..))
import Core.TextRenderer (TextRenderer(..))
import Game.AppState (AppState(..))

import qualified SDL
import qualified SDL.Font as SDL.TTF
import qualified SDL.Raw

import Control.Exception (SomeException, displayException, handle)
import Control.Monad.Reader (runReaderT)
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)

data MainContext = MainContext
  { window :: SDL.Window }

data MainState = MainState
  { screenSize :: V2 Int
  , fpsCounter :: FpsCounter.Counter
  , coreContext :: CoreContext
  , appState :: AppState
  }

main :: IO ()
main =
  handle logError $ do
    SDL.initializeAll
    SDL.TTF.initialize
    let screenSize = V2 1728 972
    window <- SDL.createWindow "" SDL.defaultWindow{ SDL.windowInitialSize = screenSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{ SDL.rendererType = SDL.AcceleratedVSyncRenderer }
    SDL.Raw.startTextInput

    font <- SDL.TTF.load fontPath 16
    cachedTextRenderer <- CachedTextRenderer.new TextRenderer{ renderer, font }
    let
      coreContext = CoreContext{ renderer, cachedTextRenderer }
      appState = AppState
        { gameState = InitialGameState.initial
        , uiState = UIState.initial screenSize
        , timeStep = Nothing
        }

    fpsCounter <- FpsCounter.new

    mainLoop
      MainContext{ window }
      MainState{ screenSize, fpsCounter, coreContext, appState }

    SDL.TTF.quit
    SDL.quit

mainLoop :: MainContext -> MainState -> IO ()
mainLoop
    ctx@MainContext{ window }
    st@MainState{ screenSize, fpsCounter, coreContext = coreContext@CoreContext{ renderer }, appState } =
  do
    case fpsCounter ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    case events & find (\case QuitEvent -> True; _ -> False) of
      Just _ -> pure ()
      Nothing -> do
        let (render, appState') = Controller.update screenSize events appState

        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer
        runReaderT render coreContext
        SDL.present renderer

        fpsCounter' <- FpsCounter.record fpsCounter
        mainLoop ctx st{ fpsCounter = fpsCounter', appState = appState' }

fontPath :: String
fontPath = "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf"

logError :: SomeException -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
