module App.Update.SlotId where

import App.Prelude

newtype SlotId = SlotId { getSlotId :: Text }
  deriving (Show, Eq)