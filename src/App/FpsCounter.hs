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

sampledFrameCount :: Num a => a
sampledFrameCount = 60

data Counter = Counter
  { counterFrequency :: Word64
  , rtsStatsEnabled :: Bool
  , frameCount :: Int
  , totalFrameTime :: Word64
  , gcAtLastUpdate :: GHC.RtsTime
  , allocatedBytesAtLastUpdate :: Word64
  , updatedText :: Maybe Text
  , lastFrameTime :: Double
  }
  deriving (Show, Generic)

data RTSStats = RTSStats
  { gcMsPerFrame :: Double
  , allocPerFrame :: Double
  , usedMemory :: Word64
  }
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
    , allocatedBytesAtLastUpdate = 0
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
updateRTS counter@Counter{ gcAtLastUpdate, allocatedBytesAtLastUpdate } = do
  GHC.RTSStats
    { GHC.gc_elapsed_ns
    , GHC.allocated_bytes
    , GHC.gc = GHC.GCDetails{ GHC.gcdetails_mem_in_use_bytes }
    } <- GHC.getRTSStats
  let rtsStats = RTSStats
        { gcMsPerFrame = fromIntegral (gc_elapsed_ns - gcAtLastUpdate) / 1000000 / sampledFrameCount
        , allocPerFrame = fromIntegral (allocated_bytes - allocatedBytesAtLastUpdate) / sampledFrameCount
        , usedMemory = gcdetails_mem_in_use_bytes
        }
      counter' = counter
        & #gcAtLastUpdate .~ gc_elapsed_ns
        & #allocatedBytesAtLastUpdate .~ allocated_bytes
  pure (counter', rtsStats)

printStats :: Counter -> Maybe RTSStats -> Text
printStats Counter{ totalFrameTime, frameCount, counterFrequency } rtsStats =
  let fps :: Double = 1 / (fromIntegral totalFrameTime / fromIntegral frameCount / fromIntegral counterFrequency)
      fps' :: Int = round fps
  in case rtsStats of
    Just RTSStats{ gcMsPerFrame, allocPerFrame, usedMemory } ->
      fromString $ printf "FPS: %d | GC: %.2f ms | alloc: %s | in use: %d MB" 
        fps' gcMsPerFrame (memToString allocPerFrame) (quot usedMemory (1024 * 1024))
    Nothing ->
      fromString $ printf "FPS: %.2f" fps

memToString :: Double -> String
memToString mem
  | mem < 1024 * 1024 = printf "%.2f kB" (mem / 1024)
  | otherwise = printf "%.2f MB" (mem / 1024 / 1024)