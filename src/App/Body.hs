module App.Body where

import App.Prelude

import App.Dims
import App.Uid (Uid)

data Body = Body
  { uid :: Uid Body
  , name :: Text
  , position :: V2 (AU Double)
  , orbitRadius :: AU Double 
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6