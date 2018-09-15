module App.Model.Resource where

import App.Prelude

data Resource
  = Mineral
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Hashable Resource
