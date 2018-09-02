module App.Model.Body where

import App.Prelude

import App.Uid (Uid)

data Body = Body
  { uid :: Uid Body
  , name :: Text
  }
  deriving (Show, Generic)

drawnRadius :: Num a => a
drawnRadius = 6