module App.Render.Rendering 
  ( Rendering, runFrame, text )
where

import App.Prelude

import qualified App.Render.TextRenderer as TextRenderer
import qualified SDL

import App.Render.RenderContext (RenderContext)
import App.Render.RenderState (RenderState(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)

type Rendering a = ReaderT RenderContext (StateT RenderState IO) a

runFrame :: RenderContext -> RenderState -> Rendering a -> IO (a, RenderState)
runFrame ctx st r = runStateT (runReaderT (r <* endFrame) ctx) st

text :: V2 CInt -> Text -> Rendering ()
text pos str = do
  renderer <- view #renderer
  texture <- zoom #textRenderer $ TextRenderer.render str
  SDL.TextureInfo{ SDL.textureWidth, SDL.textureHeight } <- SDL.queryTexture texture
  let rect = SDL.Rectangle (SDL.P pos) (V2 textureWidth textureHeight)
  SDL.copy renderer texture Nothing (Just rect)

endFrame :: Rendering ()
endFrame = do
  view #renderer >>= SDL.present
  zoom #textRenderer TextRenderer.clean
