module Core.CoreContext
  ( CoreContext(..) )
where

import App.Prelude

import Core.CachedTextRenderer (CachedTextRenderer(..))

import qualified SDL

data CoreContext = CoreContext
  { renderer :: SDL.Renderer
  , cachedTextRenderer :: CachedTextRenderer
  }
  deriving (Generic)
