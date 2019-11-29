module Game.Colonies.Building where

import GlobalImports

import qualified Game.Bodies.Resource as Resource

import Game.Bodies.Resource (Resource)
import Game.Common.Display (Display, display)

data Building
  = Mine Resource
  | Factory
  deriving (Show, Generic, Eq)

instance Hashable Building

instance Display Building where
  display = \case
    Mine resource -> display resource <> " Mine"
    Factory -> "Factory"

all :: [Building]
all =
  (Mine <$> Resource.all)
  ++ [Factory]
