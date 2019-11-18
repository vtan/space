module Main (main) where

import App.Prelude

import qualified App.Common.FpsCounter as FpsCounter
import qualified App.Logic.InitialGameState as InitialGameState
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Core.UI.Layout as Layout
import qualified Game.SystemMap.Component as SystemMap
import qualified Game.UIState as UIState

import App.Common.EventPatterns
import App.Common.Rect (Rect(..))
import Core.CoreContext (CoreContext(..))
import Core.TextRenderer (TextRenderer(..))
import Core.UI.Layout (Constrained(..))
import Core.UI.Theme
import Core.UI.UI
import Core.UI.Widgets
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
  { fpsCounter :: FpsCounter.Counter
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
        }

    fpsCounter <- FpsCounter.new

    mainLoop
      MainContext{ window }
      MainState{ fpsCounter, coreContext, appState }

    SDL.TTF.quit
    SDL.quit

mainLoop :: MainContext -> MainState -> IO ()
mainLoop
    ctx@MainContext{ window }
    st@MainState{ fpsCounter, coreContext = coreContext@CoreContext{ renderer }, appState } =
  do
    case fpsCounter ^. #updatedText of
      Just text -> SDL.windowTitle window $= text
      Nothing -> pure ()

    events <- SDL.pollEvents
    case events & find (\case QuitEvent -> True; _ -> False) of
      Just _ -> pure ()
      Nothing -> do
        let
          uiContext = UIContext
            { cursor = Rect 8 (V2 400 200)
            , defaultSize = V2 80 20
            , layoutGap = 4
            , scaleFactor = 1
            , theme = Theme
              { borderColor = V4 191 191 191 255
              , highlightColor = V4 31 171 171 255
              }
            }
          (stateChange, UIState{ renderStack }) = run uiContext events (ui appState)

        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer
        runReaderT (sequence_ (toList renderStack)) coreContext
        SDL.present renderer

        fpsCounter' <- FpsCounter.record fpsCounter
        mainLoop ctx st{ fpsCounter = fpsCounter', appState = stateChange appState }

ui :: AppState -> UIComponent AppState
ui appState =
  SystemMap.systemMap appState
  {-
  Layout.vertical
    [ Sized 20 (label gs)
    , Stretched (button "1" (const "1"))
    , Stretched $ Layout.horizontal
      [ DefaultSized (label "A")
      , DefaultSized (label "A")
      , Stretched (button "2" (const "2"))
      , Sized 40 (label "A")
      , Sized 40 (label "A")
      ]
    , DefaultSized (button "3" (const "3"))
    , DefaultSized (button "4" (const "4"))
    ]
    -}

fontPath :: String
fontPath = "data/liberation-fonts-ttf-2.00.1/LiberationSans-Regular.ttf"

logError :: SomeException -> IO ()
logError ex = do
  let message = displayException ex
  hPutStrLn stderr message
  SDL.showSimpleMessageBox Nothing SDL.Error "Exception" (fromString message)
