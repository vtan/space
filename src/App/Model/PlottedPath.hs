module App.Model.PlottedPath
  ( PlottedPath(..)
  , plot, atTime
  )
where

import App.Prelude

import qualified App.Model.Body as Body
import qualified Linear as Lin

import App.Model.Body (Body)
import App.Model.Dims (AU)
import App.Uid (Uid)

data PlottedPath = PlottedPath
  { startTime :: Int
  , startPos :: V2 (AU Double)
  , endTime :: Int
  , endPos :: V2 (AU Double)
  }
  deriving (Generic, Show)

plot :: Int -> V2 (AU Double) -> AU Double -> Uid Body -> Body -> Maybe PlottedPath
plot startTime startPos speed bodyUid rootBody = do
  (beforeCrudeApprox, _) <- approxArrival (16 * 3600) 0
  (beforeFinerApprox, _) <- approxArrival 1800 beforeCrudeApprox
  (_, endDtime) <- approxArrival 1 beforeFinerApprox
  let
    endTime = startTime + endDtime
    endPos = Body.statesAtTime endTime rootBody ^?! at bodyUid . _Just . #position
  pure PlottedPath{ startTime, startPos, endTime, endPos }
  where
    approxArrival :: Int -> Int -> Maybe (Int, Int)
    approxArrival !timeStep !dtime =
      let !reachSq =
            let reach = speed * fromIntegral dtime
            in reach * reach
          !distSq =
            let time = startTime + dtime
                pos = Body.statesAtTime time rootBody ^?! at bodyUid . _Just . #position
            in Lin.qd startPos pos
          !dtime' = dtime + timeStep
          approxTime =
            let lastReach = speed * fromIntegral (dtime - timeStep)
                lastReachSq = lastReach * lastReach
                ratio = sqrt $ (distSq - lastReachSq) / (reachSq - lastReachSq)
            in dtime - timeStep + round (ratio * fromIntegral timeStep)
      in if
        | reachSq >= distSq -> Just (dtime - timeStep, approxTime)
        | dtime' <= maxSearchTime -> approxArrival timeStep dtime'
        | otherwise -> Nothing

atTime :: Int -> PlottedPath -> V2 (AU Double)
atTime time PlottedPath{ startTime, startPos, endTime, endPos }
  | time <= startTime = startPos
  | time >= endTime = endPos
  | otherwise =
    let ratio = fromIntegral (time - startTime) / fromIntegral (endTime - startTime)
    in Lin.lerp ratio endPos startPos

maxSearchTime :: Num a => a
maxSearchTime = 2048 * 24 * 3600
