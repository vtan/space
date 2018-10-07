module App.Model.Mineral where

import App.Prelude

data Mineral = Mineral
  { available :: Int
  , accessibility :: Double
  }
  deriving (Show, Generic)
