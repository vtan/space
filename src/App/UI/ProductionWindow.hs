module App.UI.ProductionWindow where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Mineral as Mineral
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified App.Update.WidgetTree as WidgetTree

import App.Common.Util (whenAlt)
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Mineral (Mineral(..))
import App.Model.Resource (Resource)
import App.Update.Updating (Updating)
import Control.Monad.Reader.Class (local)
import Data.String (fromString)

update :: GameState -> Updating GameState
update gs@GameState{ bodies, colonies, bodyMinerals } = do
  Updating.childLayout "productionWindow" $ do
    Updating.childBounds "decoration" $ \bounds ->
      Widget.window bounds 20 "Production"

    let bodiesWithColony = toList (UidMap.zip bodies colonies)
    (selectedColony, _) <-
      Updating.childBounds "selectedBody" $ \bounds ->
        Widget.listBox
            bounds 20
            (view (_1 . #uid)) (view (_1 . #name))
            #selectedBody
            bodiesWithColony
          <&> (_1 . mapped %~ snd)

    case selectedColony of
      Just colony@Colony{ bodyUid, installations } -> do
        let minerals = bodyMinerals ^. at bodyUid . non mempty
        Updating.childLayout "rightPanel" $ do
          Updating.childLayout "installations" $ do
            let mineQty = installations ^. at Installation.Mine . non 0
            Updating.childBounds "mineCountLabel" $ \bounds ->
              Widget.label (bounds ^. #xy) "Mines"
            Updating.childBounds "mineCount" $ \bounds ->
              Widget.label (bounds ^. #xy) (fromString (show mineQty ++ " t"))

          Updating.childLayout "mining" $
            miningPanel colony minerals
          pure gs

      Nothing -> pure gs

miningPanel :: Colony -> HashMap Resource Mineral -> Updating ()
miningPanel colony minerals = do
  Updating.childBounds "header" $ \bounds ->
    Widget.label (bounds ^. #xy) "Mining"

  Updating.childLayout "mineralRows" $ do
    let allMonthlyMined = 30 *^ Logic.Colony.dailyMinedOnColony colony minerals
    ifor_ Resource.minerals $ \i resource -> do
      let Mineral{ available, accessibility } = minerals ^. at resource . Mineral.nonEmpty
          monthlyMined = allMonthlyMined ^. at resource . non 0
      Updating.childLayout "row" $ do
        rowHeight <- view (#widgetTree . #bounds . #wh . _y)
        let offsetRow bounds = bounds & #xy . _y +~ i * rowHeight
        local (#widgetTree %~ WidgetTree.mapTree (#bounds %~ offsetRow)) $ do
          Updating.childBounds "name" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ show resource)
          Updating.childBounds "monthlyMined" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ show monthlyMined ++ " t")
          Updating.childBounds "mineable" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ show available ++ " t")
          Updating.childBounds "accessibility" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ printf "%.0f%%" (100 * accessibility))
