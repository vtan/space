module App.Update.ListBoxState where

import App.Prelude

data ListBoxState i = ListBoxState
  { selectedIndex :: Maybe i
  -- TODO use unscaled
  , scrollOffset :: Int
  }
  deriving (Show, Generic)

initial :: ListBoxState i
initial = ListBoxState
  { selectedIndex = Nothing
  , scrollOffset = 0
  }
