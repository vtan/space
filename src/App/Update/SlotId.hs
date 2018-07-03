module App.Update.SlotId where

import App.Prelude

import Data.String (IsString)

newtype SlotId = SlotId { getSlotId :: Text }
  deriving (Show, Eq, IsString)