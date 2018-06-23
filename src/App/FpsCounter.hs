module App.FpsCounter 
  ( Counter
  , new, record
  )
where

import App.Prelude

import qualified GHC.Stats as GHC
import qualified SDL.Raw

import Data.String (fromString)
import Data.Word (Word64)
import Text.Printf (printf)

sampledFrameCount :: Int
sampledFrameCount = 60

data Counter = Counter
  { counterFrequency :: Word64
  , rtsStatsEnabled :: Bool
  , frameCount :: Int
  , totalFrameTime :: Word64
  , gcAtLastUpdate :: GHC.RtsTime
  , updatedText :: Maybe Text
  , lastFrameTime :: Float
  }
  deriving (Show, Generic)

-- TODO measure the stats per frame instead of per update cycle?
data RTSStats = RTSStats
  { gcMs :: Float }
  deriving (Show, Generic)

new :: IO Counter
new = do
  freq <- SDL.Raw.getPerformanceFrequency
  rtsStatsEnabled <- GHC.getRTSStatsEnabled
  pure $ Counter
    { counterFrequency = freq
    , rtsStatsEnabled = rtsStatsEnabled
    , frameCount = 0
    , totalFrameTime = 0
    , gcAtLastUpdate = 0
    , updatedText = Nothing
    , lastFrameTime = 0
    }

record :: Counter -> Word64 -> IO Counter
record counter@Counter{ counterFrequency } frameTime =
  updateIfNeeded $ counter
    & #updatedText .~ Nothing
    & #frameCount +~ 1
    & #totalFrameTime +~ frameTime
    & #lastFrameTime .~ fromIntegral frameTime / fromIntegral counterFrequency

updateIfNeeded :: Counter -> IO Counter
updateIfNeeded counter@Counter{ frameCount, rtsStatsEnabled }
  | frameCount == sampledFrameCount = do
    (counter', rtsStats) <-
      if rtsStatsEnabled
      then updateRTS counter <&> _2 %~ Just
      else pure (counter, Nothing)
    pure $ counter'
      & #updatedText .~ Just (printStats counter' rtsStats)
      & #frameCount .~ 0
      & #totalFrameTime .~ 0
  | otherwise = pure counter

updateRTS :: Counter -> IO (Counter, RTSStats)
updateRTS counter@Counter{ gcAtLastUpdate } = do
  GHC.RTSStats { GHC.gc_elapsed_ns } <- GHC.getRTSStats
  let rtsStats = RTSStats
        { gcMs = fromIntegral (gc_elapsed_ns - gcAtLastUpdate) / 1000000 }
      counter' = counter
        & #gcAtLastUpdate .~ gc_elapsed_ns
  pure (counter', rtsStats)

printStats :: Counter -> Maybe RTSStats -> Text
printStats Counter{ totalFrameTime, frameCount, counterFrequency } rtsStats =
  let fps :: Float = 1 / (fromIntegral totalFrameTime / fromIntegral frameCount / fromIntegral counterFrequency)
  in case rtsStats of
    Just RTSStats{ gcMs } ->
      fromString $ printf "FPS: %.2f | GC: %.2f ms" fps gcMs
    Nothing ->
      fromString $ printf "FPS: %.2f" fps