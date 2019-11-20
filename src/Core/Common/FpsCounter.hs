module Core.Common.FpsCounter
  ( Counter
  , new, record
  )
where

import GlobalImports

import qualified GHC.Stats as GHC
import qualified SDL.Raw

import Data.String (fromString)
import Data.Word (Word64)
import Text.Printf (printf)

sampledFrameCount :: Num a => a
sampledFrameCount = 60

data Counter = Counter
  { counterFrequency :: Word64
  , rtsStatsEnabled :: Bool
  , lastFrameEnd :: Word64
  , frameCount :: Int
  , totalFrameTime :: Word64
  , gcAtLastUpdate :: GHC.RtsTime
  , allocatedBytesAtLastUpdate :: Word64
  , updatedText :: Maybe Text
  , lastFrameTime :: Double
  }
  deriving (Show, Generic)

data RTSStats = RTSStats
  { gcSec :: Double
  , allocPerFrame :: Double
  , usedMemory :: Word64
  }
  deriving (Show, Generic)

new :: IO Counter
new = do
  freq <- SDL.Raw.getPerformanceFrequency
  now <- SDL.Raw.getPerformanceCounter
  rtsStatsEnabled <- GHC.getRTSStatsEnabled
  pure $ Counter
    { counterFrequency = freq
    , rtsStatsEnabled = rtsStatsEnabled
    , lastFrameEnd = now
    , frameCount = 0
    , totalFrameTime = 0
    , gcAtLastUpdate = 0
    , allocatedBytesAtLastUpdate = 0
    , updatedText = Nothing
    , lastFrameTime = 0
    }

record :: Counter -> IO Counter
record counter@Counter{ counterFrequency, lastFrameEnd } = do
  now <- SDL.Raw.getPerformanceCounter
  let frameTime = now - lastFrameEnd
  updateIfNeeded $ counter
    & #updatedText .~ Nothing
    & #lastFrameEnd .~ now
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
updateRTS counter@Counter{ gcAtLastUpdate, allocatedBytesAtLastUpdate } = do
  GHC.RTSStats
    { GHC.gc_elapsed_ns
    , GHC.allocated_bytes
    , GHC.gc = GHC.GCDetails{ GHC.gcdetails_mem_in_use_bytes }
    } <- GHC.getRTSStats
  let rtsStats = RTSStats
        { gcSec = fromIntegral (gc_elapsed_ns - gcAtLastUpdate) / 1000000000
        , allocPerFrame = fromIntegral (allocated_bytes - allocatedBytesAtLastUpdate) / sampledFrameCount
        , usedMemory = gcdetails_mem_in_use_bytes
        }
      counter' = counter
        & #gcAtLastUpdate .~ gc_elapsed_ns
        & #allocatedBytesAtLastUpdate .~ allocated_bytes
  pure (counter', rtsStats)

printStats :: Counter -> Maybe RTSStats -> Text
printStats Counter{ totalFrameTime, frameCount, counterFrequency } rtsStats =
  let totalFrameSec :: Double = fromIntegral totalFrameTime / fromIntegral counterFrequency
      fps = 1 / (totalFrameSec / fromIntegral frameCount)
      fps' :: Int = round fps
  in case rtsStats of
    Just RTSStats{ gcSec, allocPerFrame, usedMemory } ->
      let gcPerc = gcSec / totalFrameSec * 100
      in fromString $ printf "FPS: %d | GC: %.2f%% | alloc: %s | in use: %d MB"
        fps' gcPerc (memToString allocPerFrame) (quot usedMemory (1024 * 1024))
    Nothing ->
      fromString $ printf "FPS: %.2f" fps

memToString :: Double -> String
memToString mem
  | mem < 1024 * 1024 = printf "%.2f kB" (mem / 1024)
  | otherwise = printf "%.2f MB" (mem / 1024 / 1024)
