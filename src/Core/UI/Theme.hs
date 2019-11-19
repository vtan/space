module Core.UI.Theme where

import App.Prelude

data Theme = Theme
  { borderColor :: V4 Word8
  , highlightColor :: V4 Word8
  , backgroundColor :: V4 Word8
  , selectionBackgroundColor :: V4 Word8
  , windowDecorationColor :: V4 Word8
  }
  deriving (Generic)
