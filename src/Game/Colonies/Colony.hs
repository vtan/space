module Game.Colonies.Colony where

import GlobalImports

import Game.Bodies.Body (Body)
import Game.Bodies.Resource (Resource)
import Game.Colonies.Building (Building)
import Game.Colonies.BuildOrder (BuildOrder)
import Game.Common.Id (Id)

data Colony = Colony
  { bodyId :: Id Body
  , resources :: HashMap Resource Double
  , buildings :: HashMap Building Int
  , buildOrder :: Maybe BuildOrder
  }
  deriving (Show, Generic)
