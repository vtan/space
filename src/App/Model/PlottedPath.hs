module App.Model.PlottedPath 
  ( PlottedPath(..)
  , plot, atTime
  )
where

import App.Prelude

import qualified App.Model.OrbitSystem as OrbitSystem
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Storable as Vector
import qualified Linear as Lin

import App.Model.Body (Body)
import App.Model.Dims (AU)
import App.Model.OrbitSystem (OrbitSystem)
import App.Uid (Uid)
import Data.Vector.Storable (Vector)

data PlottedPath = PlottedPath
  { startTime :: Int 
  , startPos :: V2 (AU Double) 
  , endTime :: Int
  , endPos :: V2 (AU Double)
  , waypoints :: Vector (V2 (AU Double))
  }
  deriving (Generic, Show)

plot :: Int -> V2 (AU Double) -> AU Double -> Uid Body -> OrbitSystem -> Maybe PlottedPath
plot time position speed bodyUid orbitSystem =
  let waypointDistance = speed * waypointTime
  in plotWaypoints bodyUid orbitSystem waypointDistance (time + maxSearchTime) time position
    <&> \(furtherWaypoints, endTime) ->
      let waypoints = position :| furtherWaypoints
      in PlottedPath
        { startTime = time
        , startPos = NonEmpty.head waypoints
        , endTime = endTime
        , endPos = NonEmpty.last waypoints
        , waypoints = waypoints & toList & Vector.fromList
        }

plotWaypoints :: Uid Body -> OrbitSystem -> AU Double -> Int -> Int -> V2 (AU Double) -> Maybe ([V2 (AU Double)], Int)
plotWaypoints !bodyUid !orbitSystem !waypointDistance !maxTime !time !pos =
  let waypointDistSq = waypointDistance * waypointDistance
      bodyPos = OrbitSystem.statesAtTime time orbitSystem ^?! at bodyUid . _Just . #position
      distSq = Lin.qd pos bodyPos
  in if 
    | time >= maxTime -> Nothing
    | distSq <= waypointDistSq ->
      let ratio = sqrt $ distSq / waypointDistSq
          finalTime = round $ fromIntegral time + ratio * waypointTime
          finalPos = OrbitSystem.statesAtTime finalTime orbitSystem ^?! at bodyUid . _Just . #position
      in Just ([finalPos], finalTime)
    | otherwise ->
      let time' = time + waypointTime
          pos' = pos + waypointDistance *^ Lin.normalize (bodyPos - pos)
      in plotWaypoints bodyUid orbitSystem waypointDistance maxTime time' pos'
        <&> \(nextPositions, finalTime) -> (pos' : nextPositions, finalTime)

atTime :: Int -> PlottedPath -> V2 (AU Double)
atTime time PlottedPath{ startTime, startPos, endTime, endPos, waypoints }
  | time <= startTime = startPos
  | time >= endTime = endPos
  | otherwise =
    let i = quot (time - startTime) waypointTime
        timeOfLastWaypoint = startTime + i * waypointTime
        timeSinceLastWaypoint = time - timeOfLastWaypoint
        currentSectionTime
          | (i + 2 == Vector.length waypoints) = endTime - timeOfLastWaypoint
          | otherwise = waypointTime
        ratio = fromIntegral timeSinceLastWaypoint / fromIntegral currentSectionTime
    in case (,) <$> waypoints ^? ix i <*> waypoints ^? ix (i + 1) of
      Just (prevPos, nextPos) -> Lin.lerp ratio nextPos prevPos
      Nothing -> error "Invariant broken in PlottedPath"

waypointTime :: Num a => a
waypointTime = 24 * 3600

maxSearchTime :: Num a => a
maxSearchTime = 1024 * 24 * 3600