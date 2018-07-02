module App.Update.WidgetState where

import App.Prelude

newtype SlotId = SlotId { getSlotId :: Text }
  deriving (Show, Eq)

data TextBoxState = TextBoxState
  { slotId :: SlotId
  , text :: Text
  }
  deriving (Show, Generic)
