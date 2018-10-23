module App.Model.Installation where

import App.Prelude

data Installation
  = Infrastructure
  | Mine
  | Factory
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Hashable Installation

all :: [Installation]
all = [minBound .. maxBound]

mass :: Num a => a
mass = 500
