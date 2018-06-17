module App.Render
  ( render )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified Data.Vector.Storable as Vector
import qualified Linear.Affine as Lin
import qualified SDL as SDL

import App.Body (Body(..))
import App.Camera (Camera)
import App.Dims
import App.GameState (GameState(..))
import Data.Vector.Storable (Vector)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer GameState{ bodies, camera } = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  for_ bodies $ renderOrbit renderer camera
  SDL.present renderer

renderOrbit :: SDL.Renderer -> Camera (AU Double) Double -> Body -> IO ()
renderOrbit renderer camera Body{ position, orbitRadius } =
  let orbitPoints = circlePoints
        & Vector.map (fmap AU >>> (orbitRadius *^) >>> Camera.pointToScreen camera >>> fmap round >>> Lin.P)
      bodyCenter = Camera.pointToScreen camera position
      bodyPoints = circlePoints
        & Vector.map ((4 *^) >>> (bodyCenter +) >>> fmap round >>> Lin.P)
  in do
    SDL.rendererDrawColor renderer $= V4 0 255 255 255
    SDL.drawLines renderer orbitPoints
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.drawLines renderer bodyPoints

circlePoints :: (Floating a, Vector.Storable a) => Vector (V2 a)
circlePoints = 
  let size = 64
  in Vector.fromListN (size + 1) $ do
    n <- [0 .. size] :: [Int]
    let t = fromIntegral n / size * 2 * pi
    pure $ V2 (cos t) (sin t)