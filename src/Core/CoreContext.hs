module Core.CoreContext
  ( CoreContext(..) )
where

import GlobalImports

import Core.TextRendering.CachedTextRenderer (CachedTextRenderer(..))

import qualified SDL

data CoreContext = CoreContext
  { renderer :: SDL.Renderer
  , cachedTextRenderer :: CachedTextRenderer
  }
  deriving (Generic)
