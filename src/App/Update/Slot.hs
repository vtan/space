module App.Update.Slot 
  ( Slot, new
  , App.Update.Slot.use, App.Update.Slot.assign
  , Data.StateVar.get
  )
where

import App.Prelude

import Data.IORef
import Data.StateVar

newtype Slot a = Slot (IORef a)

instance HasGetter (Slot a) a where
  get (Slot ioRef) = get ioRef

instance HasSetter (Slot a) a where
  Slot ioRef $= x = ioRef $= x

instance HasUpdate (Slot a) a a

new :: a -> IO (Slot a)
new x = Slot <$> newIORef x

use :: (MonadState s m, MonadIO m) => Getting (Slot a) s (Slot a) -> m a
use g = App.Prelude.use g >>= get

assign :: (MonadState s m, MonadIO m) => Getting (Slot a) s (Slot a) -> a -> m ()
assign g x = App.Prelude.use g >>= ($= x)