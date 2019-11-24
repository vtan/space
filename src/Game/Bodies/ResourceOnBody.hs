module Game.Bodies.ResourceOnBody where

import GlobalImports

data ResourceOnBody = ResourceOnBody
  { available :: Double
  , accessibility :: Double
  }
  deriving (Show, Generic, Eq)

empty :: ResourceOnBody
empty = ResourceOnBody
  { available = 0
  , accessibility = 0
  }
