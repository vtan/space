module App.Update.UIState where

import App.Prelude

import qualified App.Update.Slot as Slot

import App.Ship (Ship)
import App.Uid (Uid)
import App.Update.Slot (Slot)

data UIState = UIState
  { shipWindowOpen :: Bool 
  , selectedShipUid :: Slot (Maybe (Uid Ship))
  }
  deriving (Generic)

initial :: IO UIState
initial = do
  selectedShipUid <- Slot.new Nothing
  pure $ UIState
    { shipWindowOpen = False 
    , selectedShipUid = selectedShipUid
    }