module App.Render.Render where

import App.Prelude

import qualified App.Render.TextRenderer as TextRenderer
import qualified SDL
import qualified SDL.Font as SDL.TTF

import App.Common.Rect (Rect(..))
import App.Render.TextRenderer (TextRenderer(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)

type Render a = ReaderT RenderContext (StateT RenderState IO) a

data RenderContext = RenderContext
  { renderer :: SDL.Renderer }
  deriving (Show, Generic)

data RenderState = RenderState
  { textRenderer :: TextRenderer }
  deriving (Generic)

newContext :: SDL.Renderer -> RenderContext
newContext renderer =
  RenderContext { renderer = renderer }

newState :: String -> Int -> IO RenderState
newState fontPath fontSize = do
  font <- SDL.TTF.load fontPath fontSize
  pure RenderState{ textRenderer = TextRenderer.new font }

reloadFont :: String -> Int -> RenderState -> IO RenderState
reloadFont fontPath fontSize st@RenderState{ textRenderer = TextRenderer{ font } } = do
  newFont <- SDL.TTF.load fontPath fontSize
  SDL.TTF.free font
  pure st{ textRenderer = TextRenderer.new newFont }

runFrame :: RenderContext -> RenderState -> Render a -> IO (a, RenderState)
runFrame ctx st r = runStateT (runReaderT (r <* endFrame) ctx) st
  where
    endFrame = do
      view #renderer >>= SDL.present
      TextRenderer.clean

text :: Rect Int -> Text -> Render ()
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
