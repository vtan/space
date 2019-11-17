module Core.UI.Layout
  ( Constrained(..)
  , horizontal, vertical
  )
where

import App.Prelude

import App.Common.Rect (Rect(..))
import Core.UI.UI (UIComponent, UIContext(..))

import Control.Lens (Lens')
import Data.Bifunctor (bimap)

data Constrained a
  = Sized Double a
  | Stretched a
  deriving (Generic)

horizontal :: [Constrained (UIComponent a)] -> UIComponent a
horizontal = box _x

vertical :: [Constrained (UIComponent a)] -> UIComponent a
vertical = box _y

box
  :: Lens' (V2 Double) Double
  -> [Constrained (UIComponent a)]
  -> UIComponent a
box axisLens children = do
  UIContext{ cursor } <- ask
  let
    Rect cursorPos cursorSize = cursor
    sizeToDistribute = view axisLens cursorSize

    (totalGivenSize, stretchedCount) = children
      & map (\case
          Sized size _ -> Left size
          _ -> Right ()
        )
      & partitionEithers
      & bimap sum (fromIntegral . length)
    stretchedSize =
      if stretchedCount == 0
      then 0
      else (sizeToDistribute - totalGivenSize) / stretchedCount

  (_, stateChange) <- foldlM
    ( \(usedSizeOnAxis, stateChange) constraintedChild ->
        let
          (sizeOnAxis, child) = case constraintedChild of
            Sized s c -> (s, c)
            Stretched c -> (stretchedSize, c)
          localCursor = Rect
            (over axisLens (+ usedSizeOnAxis) cursorPos)
            (set axisLens sizeOnAxis cursorSize)
          usedSizeOnAxis' = usedSizeOnAxis + sizeOnAxis
        in
          local (set #cursor localCursor) child
            & fmap (<> stateChange)
            & fmap (usedSizeOnAxis', )
    )
    (0, mempty)
    children

  pure stateChange
