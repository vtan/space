module Game.Bodies.OrbitTree where

import GlobalImports

import qualified Linear as Lin

import Game.Bodies.Body (Body(..))
import Game.Bodies.OrbitalState (OrbitalState(..))
import Game.Common.Id (Id)
import Game.Common.IdMap (IdMap)
import Game.Dimension.Local (Local(..))
import Game.Dimension.Time (Time)
import Numeric.Extras (fmod)

data OrbitTree = OrbitTree
  { bodyId :: Id Body
  , orbitRadius :: Local Double
  , phaseAtEpoch :: Double
  , angularVelocity :: Double
  , children :: [OrbitTree]
  }
  deriving (Show, Generic)

statesAtTime :: Time Int -> OrbitTree -> IdMap Body OrbitalState
statesAtTime = relStatesAtTime 0

relStatesAtTime :: V2 (Local Double) -> Time Int -> OrbitTree -> IdMap Body OrbitalState
relStatesAtTime origin time OrbitTree{ bodyId, orbitRadius, phaseAtEpoch, angularVelocity, children } =
  let phase = (phaseAtEpoch + fromIntegral time * angularVelocity) `fmod` (2 * pi)
      pos = origin + orbitRadius *^ (Local <$> Lin.angle phase)
      state = OrbitalState
        { position = pos
        , orbitCenter = origin
        }
      childStates = children & foldMap (relStatesAtTime pos time)
  in childStates & at bodyId .~ Just state
