module Game.Bodies.Body where

import GlobalImports

import Game.Bodies.Resource (Resource)
import Game.Bodies.ResourceOnBody (ResourceOnBody)
import Game.Common.Id (Id)

data Body = Body
  { bodyId :: Id Body
  , name :: Text
  , resources :: HashMap Resource ResourceOnBody
  , colonyCost :: Maybe Double
  }
  deriving (Show, Generic)
