module App.Model.Installation where

import GlobalImports

data Installation
  = Infrastructure
  | Mine
  | Factory
  | Shipyard
  deriving (Show, Generic, Eq, Enum, Bounded)

instance Hashable Installation

all :: [Installation]
all = [minBound .. maxBound]

mass :: Num a => a
mass = 500
