module App.Model.OrbitSystem where

import App.Prelude

import qualified Linear as Lin

import App.Model.Body (Body)
import App.Model.OrbitalState (OrbitalState(..))
import App.Model.Dims
import App.Uid (Uid)
import App.UidMap (UidMap)
import Numeric.Extras (fmod)

data OrbitSystem = OrbitSystem
  { body :: Uid Body
  , orbitRadius :: AU Double
  , angularVelocity :: Double
  , children :: [OrbitSystem]
  }
  deriving (Show, Generic)

subsystems :: OrbitSystem -> [OrbitSystem]
subsystems os@OrbitSystem{ children } =
  os : concatMap subsystems children

statesAtTime :: Int -> OrbitSystem -> UidMap Body OrbitalState
statesAtTime = relStatesAtTime 0

relStatesAtTime :: V2 (AU Double) -> Int -> OrbitSystem -> UidMap Body OrbitalState
relStatesAtTime origin time OrbitSystem{ body, orbitRadius, angularVelocity, children } =
  let phase = fmod (fromIntegral time * angularVelocity) (2 * pi)
      pos = origin + orbitRadius *^ (AU <$> Lin.angle phase)
      state = OrbitalState
        { position = pos
        , orbitCenter = origin
        }
      childStates = children & foldMap (relStatesAtTime pos time)
  in childStates & at body .~ Just state