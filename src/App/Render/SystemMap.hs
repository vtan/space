module App.Render.SystemMap
  ( render )
where

import App.Prelude

import qualified App.Common.Camera as Camera
import qualified App.Model.Body as Body
import qualified App.Model.Ship as Ship
import qualified App.Render.Rendering as Rendering
import qualified Data.Vector.Storable as Vector
import qualified Linear as Linear
import qualified SDL

import App.Common.Camera (Camera(..))
import App.Common.UidMap (UidMap)
import App.Dimension.Local (Local(..))
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.PlottedPath (PlottedPath(..))
import App.Model.Ship (Ship(..))
import App.Render.Rendering (Rendering)
import Data.Vector.Storable (Vector)
import SDL (($=))

render :: Camera (Local Double) Double -> GameState -> Rendering ()
render camera GameState{ rootBody, bodyOrbitalStates, ships } = do
  renderer <- view #renderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  _ <- renderBody camera bodyOrbitalStates Nothing rootBody
  for_ ships $ renderShip camera

renderBody :: Camera (Local Double) Double -> UidMap Body OrbitalState -> Maybe (V2 Double) -> Body -> Rendering Bool
renderBody camera orbitalStates parentDrawnCenter Body{ uid = bodyUid, name = bodyName, orbitRadius, children } =
  case orbitalStates ^. at bodyUid of
    Just OrbitalState{ position, orbitCenter } -> do
      let camCenter = Camera.screenToPoint camera (camera ^. #eyeTo)
          camRadiusSq = Camera.boundingCircleRadiusSq camera
          bodyCenter = Camera.pointToScreen camera position
          bodyVisible = True -- TODO cull invisible bodies
          orbitVisible = Linear.distance orbitCenter camCenter < orbitRadius + sqrt camRadiusSq
          drawOrbit =
            orbitVisible
            && case parentDrawnCenter of
              Just parentCenter -> Linear.qd bodyCenter parentCenter > 8 * 8
              Nothing -> True
          drawBody =
            bodyVisible
            && case parentDrawnCenter of
              Just parentCenter -> Linear.qd bodyCenter parentCenter > 16 * 16
              Nothing -> True
      renderer <- view #renderer
      when drawOrbit $ do
        let orbitPoints = orbitCircle
              & Vector.map (fmap Local >>> (orbitRadius *^) >>> (orbitCenter +) >>> Camera.pointToScreen camera >>> fmap round >>> SDL.P)
        SDL.rendererDrawColor renderer $= V4 0 255 255 255
        SDL.drawLines renderer orbitPoints
      when drawBody $ do
        let bodyPoints = bodyCircle
              & Vector.map ((Body.drawnRadius *^) >>> (bodyCenter +) >>> fmap round >>> SDL.P)
        SDL.rendererDrawColor renderer $= V4 255 255 255 255
        SDL.drawLines renderer bodyPoints
        childrenVisible <- for children (renderBody camera orbitalStates (Just bodyCenter))
        let label
              | all id childrenVisible = bodyName
              | otherwise = bodyName <> "..."
            labelPos = bodyCenter & _y +~ 8
        Rendering.text (fmap floor labelPos) label
      pure drawBody
    Nothing -> pure False

renderShip :: Camera (Local Double) Double -> Ship -> Rendering ()
renderShip camera Ship{ Ship.position, order } =
  let center = Camera.pointToScreen camera position
      points = shipCircle
        & Vector.map ((Ship.drawnRadius *^) >>> (center +) >>> fmap round >>> SDL.P)
  in do
    renderer <- view #renderer
    SDL.rendererDrawColor renderer $= V4 255 255 0 255
    SDL.drawLines renderer points
    case order of
      Just Ship.MoveToBody{ Ship.path = PlottedPath{ startPos, endPos } } -> do
        SDL.rendererDrawColor renderer $= V4 255 255 255 255
        let startPos' = startPos & (Camera.pointToScreen camera >>> fmap round >>> SDL.P)
            endPos' = endPos & (Camera.pointToScreen camera >>> fmap round >>> SDL.P)
        SDL.drawLine renderer startPos' endPos'
      Nothing -> pure ()

bodyCircle, shipCircle, orbitCircle :: Vector (V2 Double)
bodyCircle = circle 16
shipCircle = circle 4
orbitCircle = circle 64

circle :: Int -> Vector (V2 Double)
circle size =
  Vector.fromListN (size + 1) $ do
    n <- [0 .. size] :: [Int]
    let t = fromIntegral n / fromIntegral size * 2 * pi
    pure $ V2 (cos t) (sin t)
