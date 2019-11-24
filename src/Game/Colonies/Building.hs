module Game.Colonies.Building where

import GlobalImports

import qualified Game.Bodies.Resource as Resource

import Game.Bodies.Resource (Resource)

data Building
  = Mine Resource
  deriving (Show, Generic, Eq)

instance Hashable Building

all :: [Building]
all = Mine <$> Resource.all
