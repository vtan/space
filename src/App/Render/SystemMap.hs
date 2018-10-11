module App.Render.SystemMap
  ( render )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Model.Body as Body
import qualified App.Model.OrbitalState as OrbitalState
import qualified App.Model.Ship as Ship
import qualified App.Render.Rendering as Rendering
import qualified App.UidMap as UidMap
import qualified Data.Vector.Storable as Vector
import qualified Linear.Affine as Lin
import qualified SDL

import App.Camera (Camera(..))
import App.Model.Body (Body(..))
import App.Model.Dims
import App.Model.GameState (GameState(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.PlottedPath (PlottedPath(..))
import App.Model.Ship (Ship(..))
import App.Render.Rendering (Rendering)
import App.Util (toMap)
import Data.Vector.Storable (Vector)
import SDL (($=))

render :: Camera (AU Double) Double -> GameState -> Rendering ()
render camera gs@GameState{ rootBody, bodyOrbitalStates, ships } = do
  renderer <- view #renderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer

  for_ (Body.allChildren rootBody) $ \body ->
    for_ (bodyOrbitalStates ^. at (body ^. #uid)) $ \st ->
      renderOrbit camera body st
  for_ ships $ renderShip camera
  ifor_ (collectLabels camera gs & toMap) $ \anchor labels ->
    ifor_ labels $ \row label ->
      let pos = anchor & _y +~ 8 + row * 16
      in Rendering.text pos label

renderOrbit :: Camera (AU Double) Double -> Body -> OrbitalState -> Rendering ()
renderOrbit camera Body{ orbitRadius } OrbitalState{ OrbitalState.position, orbitCenter } =
  let orbitPoints = circlePoints
        & Vector.map (fmap AU >>> (orbitRadius *^) >>> (orbitCenter +) >>> Camera.pointToScreen camera >>> fmap round >>> Lin.P)
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
      Just Ship.MoveToBody{ Ship.path = PlottedPath{ startPos, endPos } } -> do
        SDL.rendererDrawColor renderer $= V4 255 255 255 255
        let startPos' = startPos & (Camera.pointToScreen camera >>> fmap round >>> Lin.P)
            endPos' = endPos & (Camera.pointToScreen camera >>> fmap round >>> Lin.P)
        SDL.drawLine renderer startPos' endPos'
      Nothing -> pure ()

collectLabels :: Camera (AU Double) Double -> GameState -> [(V2 Int, [Text])]
collectLabels camera GameState{ bodies, bodyOrbitalStates, ships } = bodyLabels ++ shipLabels
  where
    bodyLabels = (UidMap.zip bodies bodyOrbitalStates) & foldMap (\(Body{ Body.name }, OrbitalState{ OrbitalState.position }) ->
        let pos = position & Camera.pointToScreen camera & fmap round
        in [(pos, [name])]
      )
    shipLabels = ships & foldMap (\Ship{ Ship.position, Ship.name } ->
        let pos = position & Camera.pointToScreen camera & fmap round
        in [(pos, [name])]
      )

circlePoints :: (Floating a, Vector.Storable a) => Vector (V2 a)
circlePoints =
  let size = 64
  in Vector.fromListN (size + 1) $ do
    n <- [0 .. size] :: [Int]
    let t = fromIntegral n / size * 2 * pi
    pure $ V2 (cos t) (sin t)
