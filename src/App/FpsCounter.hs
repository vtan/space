module App.FpsCounter (Counter, new, record) where

import App.Prelude

import qualified SDL.Raw

import Data.Word (Word64)

sampledFrameCount :: Int
sampledFrameCount = 20

data Counter = Counter
  { counterFrequency :: Word64
  , frameCount :: Int
  , counterSum :: Word64
  , updatedFps :: Maybe Float
  , lastFrameTime :: Float
  }
  deriving (Show, Generic)

new :: IO Counter
new = do
  freq <- SDL.Raw.getPerformanceFrequency
  pure $ Counter
    { counterFrequency = freq
    , frameCount = 0
    , counterSum = 0
    , updatedFps = Nothing
    , lastFrameTime = 0
    }

record :: Counter -> Word64 -> Counter
record counter lastFrame =
  counter
    & over #frameCount (+ 1)
    & over #counterSum (+ lastFrame)
    & set #updatedFps Nothing
    -- TODO: Might as well store the sum in seconds too
    & set #lastFrameTime (fromIntegral lastFrame / fromIntegral (counter ^. #counterFrequency))
    & updateIfNeeded

updateIfNeeded :: Counter -> Counter
updateIfNeeded counter
  | count == sampledFrameCount =
      counter
        & set #frameCount 0
        & set #counterSum 0
        & set #updatedFps (Just $ 1 / (fromIntegral s / fromIntegral count / fromIntegral freq))
  | otherwise = counter
  where
    Counter{ counterFrequency = freq, frameCount = count, counterSum = s } = counter
