module Game.SystemMap.Rendering
  ( render )
where

import GlobalImports

import qualified Core.Common.Rect as Rect
import qualified Core.TextRendering.CachedTextRenderer as CachedTextRenderer
import qualified Core.TextRendering.RenderedText as RenderedText
import qualified Game.Common.Camera as Camera

import Core.CoreContext (CoreContext(..))
import Core.TextRendering.CachedTextRenderer (CachedTextRenderer(..))
import Game.GameState (GameState(..))
import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitTree (OrbitTree(..))
import Game.Bodies.OrbitalState (OrbitalState(..))
import Game.Common.Camera (Camera(..))
import Game.Common.IdMap (IdMap)
import Game.Dimension.Local (Local(..))
import Game.Ships.Ship (Ship(..))

import qualified Data.Vector.Storable as Vector
import qualified Linear as Linear
import qualified SDL

import Data.Vector.Storable (Vector)
import SDL (($=))

type Render a = ReaderT CoreContext IO a

render :: Camera (Local Double) Double -> GameState -> Render ()
render camera GameState{ orbitTree, bodyOrbitalStates, bodies, ships } = do
  _ <- renderBodyEnv camera bodyOrbitalStates bodies >>= \env -> renderBody env Nothing orbitTree
  for_ ships (renderShip camera)

data RenderBodyEnv = RenderBodyEnv
  { renderer :: SDL.Renderer
  , cachedTextRenderer :: CachedTextRenderer
  , camera :: Camera (Local Double) Double
  , cameraCenter :: V2 (Local Double)
  , cameraRadiusSq :: Local Double
  , cameraRadius :: Local Double
  , orbitalStates :: IdMap Body OrbitalState
  , bodies :: IdMap Body Body
  }

renderBodyEnv :: Camera (Local Double) Double -> IdMap Body OrbitalState -> IdMap Body Body -> Render RenderBodyEnv
renderBodyEnv camera orbitalStates bodies = do
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
    , bodies
    }

renderBody :: RenderBodyEnv -> Maybe (V2 Double) -> OrbitTree -> Render Bool
renderBody
    env@RenderBodyEnv{ renderer, cachedTextRenderer, camera, cameraCenter, cameraRadiusSq, cameraRadius, orbitalStates, bodies }
    parentDrawnCenter
    OrbitTree{ bodyId, orbitRadius, children } =
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
          !bodyName = fromMaybe "???" (preview (at bodyId . _Just . #name) bodies)
      when drawOrbit (renderVisibleOrbit orbitCenter)
      when drawBody (renderVisibleBody bodyCenter)
      childrenVisible <- for children (renderBody env (Just bodyCenter))
      when drawBody (renderBodyLabel bodyName bodyCenter childrenVisible)
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
            & Vector.map ((bodyRadius *^) >>> (bodyCenter +) >>> fmap round >>> SDL.P)
      SDL.rendererDrawColor renderer $= bodyColor
      SDL.drawLines renderer bodyPoints

    renderBodyLabel bodyName bodyCenter childrenVisible = do
      let label
            | all id childrenVisible = bodyName
            | otherwise = bodyName <> "..."
          labelRect = Rect.fromMinSize (bodyCenter & _y +~ 8) (V2 256 40)
      renderedText <- cachedTextRenderer & CachedTextRenderer.render label
      RenderedText.render renderer 1 labelRect renderedText

renderShip :: Camera (Local Double) Double -> Ship -> Render ()
renderShip camera Ship{ position } = do
  let center = Camera.pointToScreen camera position
      points = shipCircle
        & Vector.map ((shipRadius *^) >>> (center +) >>> fmap round >>> SDL.P)
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

bodyRadius :: Double
bodyRadius = 6

shipRadius :: Double
shipRadius = 6

orbitColor, bodyColor, shipColor :: V4 Word8
orbitColor = V4 0x00 0xcf 0xcf 0xff
bodyColor = V4 0xcf 0xcf 0xcf 0xff
shipColor = V4 0xff 0xff 0x00 0xff
