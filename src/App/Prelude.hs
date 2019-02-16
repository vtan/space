module App.Prelude
  ( module X
  , TextBuilder
  )
where

import Prelude as X
import Control.Applicative as X
import Control.Arrow as X
import Control.Lens.At as X
import Control.Lens.Cons as X
import Control.Lens.Each as X
import Control.Lens.Empty as X
import Control.Lens.Getter as X
import Control.Lens.Fold as X
import Control.Lens.Indexed as X
import Control.Lens.Iso as X
import Control.Lens.Operators as X
import Control.Lens.Prism as X
import Control.Lens.Setter as X
import Control.Lens.Tuple as X
import Control.Lens.Traversal as X
import Control.Monad as X
import Control.Monad.Zip as X
import Data.Either as X
import Data.Foldable as X
import Data.Function as X
import Data.Maybe as X
import Data.Traversable as X

import Control.Monad.IO.Class as X
  (MonadIO)
import Control.Monad.Reader as X
  (MonadReader, ReaderT)
import Control.Monad.State.Strict as X
  (MonadState, StateT)
import Data.Generics.Labels as X
  ()
import Data.HashMap.Strict as X
  (HashMap)
import Data.HashSet as X
  (HashSet)
import Data.Hashable as X
  (Hashable)
import Data.IntMap.Strict as X
  (IntMap)
import Data.List.NonEmpty as X
  (NonEmpty((:|)))
import Data.Semigroup as X
  ((<>), Any(..), First(..), Sum(..))
import Data.Word as X
  (Word8, Word32)
import Data.StateVar as X
  (($=))
import Data.Text as X
  (Text)
import Debug.Trace as X
  (traceShowId)
import Foreign.C.Types as X
  (CInt)
import Foreign.Storable as X
  (Storable)
import GHC.Generics as X
  (Generic)
import Linear as X
  (V2(..), V4(..), (*^), (^/), _x, _y)

import qualified Data.Text.Lazy.Builder

type TextBuilder = Data.Text.Lazy.Builder.Builder
