module App.Ship where

import App.Prelude

import App.Body (Body)
import App.Dims (AU)
import App.PlottedPath (PlottedPath)
import App.Uid (Uid)

data Ship = Ship
  { uid :: Uid Ship
  , name :: Text
  , position :: V2 (AU Double)
  , speed :: AU Double
  , order :: Maybe Order
  , attachedToBody :: Maybe (Uid Body)
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6

data Order
  = MoveToBody 
    { bodyUid :: Uid Body
    , path :: PlottedPath
    }
  deriving (Show, Generic)