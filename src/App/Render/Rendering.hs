module App.Render.Rendering 
  ( Rendering, run )
where

import App.Prelude

import App.Render.Context (Context)
import Control.Monad.Reader

newtype Rendering a = Rendering (ReaderT Context IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

run :: Context -> Rendering a -> IO a
run ctx (Rendering r) = runReaderT r ctx