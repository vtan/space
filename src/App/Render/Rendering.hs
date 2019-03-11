module App.Render.Rendering where

import App.Prelude

import qualified App.Render.TextRenderer as TextRenderer
import qualified SDL
import qualified SDL.Font as SDL.TTF

import App.Common.Rect (Rect(..))
import App.Render.TextRenderer (TextRenderer(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)

type Rendering a = ReaderT Context (StateT State IO) a

data Context = Context
  { renderer :: SDL.Renderer }
  deriving (Show, Generic)

data State = State
  { textRenderer :: TextRenderer }
  deriving (Generic)

newContext :: SDL.Renderer -> Context
newContext renderer =
  Context { renderer = renderer }

newState :: String -> Int -> IO State
newState fontPath fontSize = do
  font <- SDL.TTF.load fontPath fontSize
  pure State{ textRenderer = TextRenderer.new font }

reloadFont :: String -> Int -> State -> IO State
reloadFont fontPath fontSize st@State{ textRenderer = TextRenderer{ font } } = do
  newFont <- SDL.TTF.load fontPath fontSize
  SDL.TTF.free font
  pure st{ textRenderer = TextRenderer.new newFont }

runFrame :: Context -> State -> Rendering a -> IO (a, State)
runFrame ctx st r = runStateT (runReaderT (r <* endFrame) ctx) st
  where
    endFrame = do
      view #renderer >>= SDL.present
      TextRenderer.clean

text :: Rect Int -> Text -> Rendering ()
text (Rect pos requestedSize) str =
  case str of
    Empty -> pure ()
    _ -> do
      renderer <- view #renderer
      TextRenderer.RenderedText{ texture, size = textureSize } <-
        TextRenderer.render renderer str
      let actualSize = min <$> fmap fromIntegral requestedSize <*> textureSize
          sourceRect = SDL.Rectangle 0 actualSize
          destinationRect = SDL.Rectangle (SDL.P $ fmap fromIntegral pos) actualSize
      SDL.copy renderer texture (Just sourceRect) (Just destinationRect)
