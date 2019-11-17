module Core.UI.Theme where

import App.Prelude

data Theme = Theme
  { borderColor :: V4 Word8 }
  deriving (Generic)
