module App.Model.Installation where

import App.Prelude

data Installation
  = Mine
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Hashable Installation
