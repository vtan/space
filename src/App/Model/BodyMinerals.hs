module App.Model.BodyMinerals where

import App.Prelude

type Mineral = Int

type BodyMinerals = HashMap Mineral MineralData

data MineralData = MineralData
  { available :: Double
  , accessibility :: Double
  }
  deriving (Show, Generic)