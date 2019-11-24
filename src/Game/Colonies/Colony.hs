module Game.Colonies.Colony where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Common.Id (Id)
import Game.Colonies.Building (Building)
import Game.Bodies.Resource (Resource)

data Colony = Colony
  { bodyId :: Id Body
  , resources :: HashMap Resource Double
  , buildings :: HashMap Building Int
  }
  deriving (Show, Generic)
