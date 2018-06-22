module App.Body where

import App.Prelude

import qualified Linear as Lin

import App.Dims
import App.Uid (Uid)
import Numeric.Extras (fmod)

data Body = Body
  { uid :: Uid Body
  , name :: Text
  , orbitRadius :: AU Double 
  , angularVelocity :: Double
  , phase :: Double
  , position :: V2 (AU Double)
  }
  deriving (Show, Generic)

new :: Uid Body -> Text -> AU Double -> Double -> Body
new uid name orbitRadius angularVelocity = Body
  { uid = uid
  , name = name
  , orbitRadius = orbitRadius
  , angularVelocity = angularVelocity
  , phase = 0
  , position = V2 orbitRadius 0
  }

atTime :: Int -> Body -> Body
atTime time body@Body{ orbitRadius, angularVelocity } =
  let phase' = fmod (fromIntegral time * angularVelocity) (2 * pi)
      position' = orbitRadius *^ (AU <$> Lin.angle phase')
  in body
    { phase = phase'
    , position = position'
    }

drawnRadius :: Num a => a
drawnRadius = 6