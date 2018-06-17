module App.Body where

import App.Prelude

import App.Dims

data Body = Body
  { position :: V2 (AU Double)
  , orbitRadius :: AU Double 
  }
  deriving (Show, Generic)