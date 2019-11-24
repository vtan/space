module Core.UI.Layout
  ( Constrained(..)
  , horizontal, vertical
  , indent
  )
where

import GlobalImports

import qualified Core.UI.UI as UI

import Core.Common.Rect (Rect(..))
import Core.UI.UI (UIComponent, UIContext(..))

import Control.Lens (Lens')
import Data.Bifunctor (bimap)

data Constrained a
  = Sized Double a
  | DefaultSized a
  | Stretched a
  deriving (Generic)

horizontal :: [Constrained (UIComponent a)] -> UIComponent a
horizontal = box _x

vertical :: [Constrained (UIComponent a)] -> UIComponent a
vertical = box _y

indent :: Double -> UIComponent a -> UIComponent a
indent indentation child =
  horizontal [Sized indentation UI.empty, Stretched child]

box
  :: Lens' (V2 Double) Double
  -> [Constrained (UIComponent a)]
  -> UIComponent a
box axisLens children = do
  UIContext{ cursor, defaultSize, layoutGap } <- ask
  let
    defaultSizeOnAxis =  view axisLens defaultSize
    Rect cursorPos cursorSize = cursor
    sizeToDistribute =
      view axisLens cursorSize
      - fromIntegral (length children - 1) * layoutGap

    (totalGivenSize, stretchedCount) = children
      & map (\case
          Sized size _ -> Left size
          DefaultSized _ -> Left defaultSizeOnAxis
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
            DefaultSized c -> (defaultSizeOnAxis, c)
            Stretched c -> (stretchedSize, c)
          localCursor = Rect
            (over axisLens (+ usedSizeOnAxis) cursorPos)
            (set axisLens sizeOnAxis cursorSize)
          usedSizeOnAxis' = usedSizeOnAxis + sizeOnAxis + layoutGap
        in
          local (set #cursor localCursor) child
            & fmap (<> stateChange)
            & fmap (usedSizeOnAxis', )
    )
    (0, mempty)
    children

  pure stateChange
