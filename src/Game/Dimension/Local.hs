module Game.Dimension.Local where

import GlobalImports

import qualified Control.Lens as Lens

-- | Distance local to a star system.
newtype Local a = Local { toAU :: a }
  deriving ( Generic, Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, Storable )

au :: a -> Local a
au = Local

iso :: Lens.Iso (Local a) (Local b) a b
iso = Lens.iso toAU Local
