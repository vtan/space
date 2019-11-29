module Core.UI.Theme where

import GlobalImports

data Theme = Theme
  { borderColor :: V4 Word8
  , highlightColor :: V4 Word8
  , backgroundColor :: V4 Word8
  , widgetBackgroundColor :: V4 Word8
  , selectionBackgroundColor :: V4 Word8
  , windowDecorationColor :: V4 Word8
  }
  deriving (Generic)

theme :: Theme
theme = Theme
  { borderColor = V4 91 91 91 255
  , highlightColor = V4 31 171 171 255
  , backgroundColor = V4 31 31 31 255
  , widgetBackgroundColor = V4 47 47 47 255
  , selectionBackgroundColor = V4 31 171 171 255
  , windowDecorationColor = V4 91 91 91 255
  }
