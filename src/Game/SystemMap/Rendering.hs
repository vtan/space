module Game.SystemMap.Rendering
  ( render )
where

import App.Prelude

import qualified App.Common.Camera as Camera
import qualified App.Common.Rect as Rect
import qualified App.Model.Body as Body
import qualified App.Model.Ship as Ship
import qualified Core.CachedTextRenderer as CachedTextRenderer
import qualified Core.RenderedText as RenderedText

import App.Common.Camera (Camera(..))
import App.Common.IdMap (IdMap)
import App.Dimension.Local (Local(..))
import App.Model.Body (Body(..))
import App.Model.GameState (GameState(..))
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Ship (Ship(..))
import Core.CachedTextRenderer (CachedTextRenderer(..))
import Core.CoreContext (CoreContext(..))

import qualified Data.Vector.Storable as Vector
import qualified Linear as Linear
import qualified SDL

import Data.Vector.Storable (Vector)
import SDL (($=))

type Render a = ReaderT CoreContext IO a

render :: Camera (Local Double) Double -> GameState -> Render ()
render camera GameState{ rootBody, bodyOrbitalStates, ships } = do
  _ <- renderBodyEnv camera bodyOrbitalStates >>= \env -> renderBody env Nothing rootBody
  for_ ships (renderShip camera)

data RenderBodyEnv = RenderBodyEnv
  { renderer :: SDL.Renderer
  , cachedTextRenderer :: CachedTextRenderer
  , camera :: Camera (Local Double) Double
  , cameraCenter :: V2 (Local Double)
  , cameraRadiusSq :: Local Double
  , cameraRadius :: Local Double
  , orbitalStates :: IdMap Body OrbitalState
  }

renderBodyEnv :: Camera (Local Double) Double -> IdMap Body OrbitalState -> Render RenderBodyEnv
renderBodyEnv camera orbitalStates = do
  CoreContext{ renderer, cachedTextRenderer } <- ask
  let cameraRadiusSq = Camera.boundingCircleRadiusSq camera
  pure RenderBodyEnv
    { renderer
    , cachedTextRenderer
    , camera
    , cameraCenter = Camera.screenToPoint camera (camera ^. #eyeTo)
    , cameraRadiusSq
    , cameraRadius = sqrt cameraRadiusSq
    , orbitalStates
    }

renderBody :: RenderBodyEnv -> Maybe (V2 Double) -> Body -> Render Bool
renderBody
    env@RenderBodyEnv{ renderer, cachedTextRenderer, camera, cameraCenter, cameraRadiusSq, cameraRadius, orbitalStates }
    parentDrawnCenter
    Body{ bodyId, name = bodyName, orbitRadius, children } =
  case orbitalStates ^. at bodyId of
    Just OrbitalState{ position, orbitCenter } -> do
      let !bodyCenter = Camera.pointToScreen camera position
          (!drawOrbit, !drawBody) =
            let !bodyVisible = Linear.qd position cameraCenter < cameraRadiusSq
                !orbitVisible = Linear.distance orbitCenter cameraCenter < orbitRadius + cameraRadius
            in case parentDrawnCenter of
              Just parentCenter ->
                let !bodyParentQd = Linear.qd bodyCenter parentCenter
                in (orbitVisible && bodyParentQd > 8 * 8, bodyVisible && bodyParentQd > 16 * 16)
              Nothing ->
                (orbitVisible, bodyVisible)
      when drawOrbit (renderVisibleOrbit orbitCenter)
      when drawBody (renderVisibleBody bodyCenter)
      childrenVisible <- for children (renderBody env (Just bodyCenter))
      when drawBody (renderBodyLabel bodyCenter childrenVisible)
      pure drawBody
    Nothing -> pure False
  where
    renderVisibleOrbit orbitCenter = do
      let orbitPoints = orbitCircle
            & Vector.map (fmap Local >>> (orbitRadius *^) >>> (orbitCenter +) >>> Camera.pointToScreen camera >>> fmap floor >>> SDL.P)
      SDL.rendererDrawColor renderer $= orbitColor
      SDL.drawLines renderer orbitPoints

    renderVisibleBody bodyCenter = do
      let bodyPoints = bodyCircle
            & Vector.map ((Body.drawnRadius *^) >>> (bodyCenter +) >>> fmap round >>> SDL.P)
      SDL.rendererDrawColor renderer $= bodyColor
      SDL.drawLines renderer bodyPoints

    renderBodyLabel bodyCenter childrenVisible = do
      let label
            | all id childrenVisible = bodyName
            | otherwise = bodyName <> "..."
          labelRect = Rect.fromMinSize (floor <$> (bodyCenter & _y +~ 8)) (V2 256 40)
      renderedText <- cachedTextRenderer & CachedTextRenderer.render label
      RenderedText.render renderer labelRect renderedText

renderShip :: Camera (Local Double) Double -> Ship -> Render ()
renderShip camera Ship{ Ship.position } = do
  let center = Camera.pointToScreen camera position
      points = shipCircle
        & Vector.map ((Ship.drawnRadius *^) >>> (center +) >>> fmap round >>> SDL.P)
  renderer <- view #renderer
  SDL.rendererDrawColor renderer $= shipColor
  SDL.drawLines renderer points

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

orbitColor, bodyColor, shipColor :: V4 Word8
orbitColor = V4 0x00 0xcf 0xcf 0xff
bodyColor = V4 0xcf 0xcf 0xcf 0xff
shipColor = V4 0xff 0xff 0x00 0xff
