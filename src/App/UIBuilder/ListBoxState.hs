module App.UIBuilder.ListBoxState where

import App.Prelude

import App.UIBuilder.Unscaled

data ListBoxState i = ListBoxState
  { selectedIndex :: Maybe i
  , scrollOffset :: Unscaled Int
  }
  deriving (Show, Generic)

initial :: ListBoxState i
initial = ListBoxState
  { selectedIndex = Nothing
  , scrollOffset = 0
  }
