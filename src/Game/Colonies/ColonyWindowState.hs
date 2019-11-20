module Game.Colonies.ColonyWindowState
  ( ColonyWindowState(..)
  , initial
  )
where

import GlobalImports

import App.Model.Body (Body)
import Game.Common.Id (Id)

data ColonyWindowState = ColonyWindowState
  { selectedBodyId :: Maybe (Id Body) }
  deriving (Generic)

initial :: ColonyWindowState
initial = ColonyWindowState
  { selectedBodyId = Nothing }
