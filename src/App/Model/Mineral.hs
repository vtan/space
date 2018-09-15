module App.Model.Mineral where

import App.Prelude

data Mineral = Mineral
  { available :: Double
  , accessibility :: Double
  }
  deriving (Show, Generic)
