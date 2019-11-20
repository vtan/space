module Game.ColonyWindowState
  ( ColonyWindowState(..)
  , initial
  )
where

import GlobalImports

import Game.Common.Id (Id)
import App.Model.Body (Body)

data ColonyWindowState = ColonyWindowState
  { selectedBodyId :: Maybe (Id Body) }
  deriving (Generic)

initial :: ColonyWindowState
initial = ColonyWindowState
  { selectedBodyId = Nothing }
