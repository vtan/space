module App.Render
  ( render )
where

import App.Prelude

import qualified App.Body as Body
import qualified App.Camera as Camera
import qualified App.Render.Rendering as Rendering
import qualified App.Ship as Ship
import qualified Data.Vector.Storable as Vector
import qualified Linear.Affine as Lin
import qualified SDL as SDL

import App.Body (Body(..))
import App.Camera (Camera)
import App.Dims
import App.GameState (GameState(..))
import App.PlottedPath (PlottedPath(..))
import App.Render.Rendering (Rendering)
import App.Ship (Ship(..))
import App.Util (showDate)
import Data.String (fromString)
import Data.Vector.Storable (Vector)
import SDL (($=))

render :: GameState -> Rendering ()
render GameState{ bodies, ships, time, camera } = do
  renderer <- view #renderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  for_ bodies $ renderOrbit camera
  for_ ships $ renderShip camera
  Rendering.text (V2 8 8) (fromString $ showDate time)

renderOrbit :: Camera (AU Double) Double -> Body -> Rendering ()
renderOrbit camera Body{ Body.position, orbitRadius } =
  let orbitPoints = circlePoints
        & Vector.map (fmap AU >>> (orbitRadius *^) >>> Camera.pointToScreen camera >>> fmap round >>> Lin.P)
      bodyCenter = Camera.pointToScreen camera position
      bodyPoints = circlePoints
        & Vector.map ((Body.drawnRadius *^) >>> (bodyCenter +) >>> fmap round >>> Lin.P)
  in do
    renderer <- view #renderer
    SDL.rendererDrawColor renderer $= V4 0 255 255 255
    SDL.drawLines renderer orbitPoints
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.drawLines renderer bodyPoints

renderShip :: Camera (AU Double) Double -> Ship -> Rendering ()
renderShip camera Ship{ Ship.position, order } =
  let center = Camera.pointToScreen camera position
      points = circlePoints
        & Vector.map ((Ship.drawnRadius *^) >>> (center +) >>> fmap round >>> Lin.P)
  in do
    renderer <- view #renderer
    SDL.rendererDrawColor renderer $= V4 255 255 0 255
    SDL.drawLines renderer points
    case order of
      Just Ship.MoveToBody{ Ship.path = PlottedPath{ waypoints } } -> do
        SDL.rendererDrawColor renderer $= V4 255 255 255 255
        let pathPoints = waypoints & Vector.map (Camera.pointToScreen camera >>> fmap round >>> Lin.P)
        SDL.drawLines renderer pathPoints
      Nothing -> pure ()

circlePoints :: (Floating a, Vector.Storable a) => Vector (V2 a)
circlePoints = 
  let size = 64
  in Vector.fromListN (size + 1) $ do
    n <- [0 .. size] :: [Int]
    let t = fromIntegral n / size * 2 * pi
    pure $ V2 (cos t) (sin t)