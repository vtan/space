module App.UI.ProductionWindow
  ( update )
where

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

data Action
  = ChangePriority Resource Int

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

          miningAction <- Updating.childLayout "mining" $
            miningPanel colony minerals

          pure $ case miningAction of
            Just (ChangePriority resource diff) ->
              Logic.Colony.changeMiningPriority resource diff colony gs
            Nothing -> gs

      Nothing -> pure gs

miningPanel :: Colony -> HashMap Resource Mineral -> Updating (Maybe Action)
miningPanel colony@Colony{ miningPriorities } minerals = do
  Updating.childBounds "heading" $ \bounds ->
    Widget.label (bounds ^. #xy) "Mining"

  Updating.childLayout "headerRow" $ do
    Updating.childBounds "monthlyMined" $ \bounds ->
      Widget.label (bounds ^. #xy) "Mined / mo"
    Updating.childBounds "priority" $ \bounds ->
      Widget.label (bounds ^. #xy) "Priority"
    Updating.childBounds "mineable" $ \bounds ->
      Widget.label (bounds ^. #xy) "Mineable"
    Updating.childBounds "accessibility" $ \bounds ->
      Widget.label (bounds ^. #xy) "Accessibility"

  Updating.childLayout "mineralRows" $ do
    let allMonthlyMined = 30 *^ Logic.Colony.dailyMinedOnColony colony minerals
    actions <- ifor Resource.minerals $ \i resource -> do
      let Mineral{ available, accessibility } = minerals ^. at resource . Mineral.nonEmpty
          monthlyMined = allMonthlyMined ^. at resource . non 0
          priority = miningPriorities ^. at resource . non 0
      Updating.childLayout "row" $ do
        rowHeight <- view (#widgetTree . #bounds . #wh . _y)
        let offsetRow bounds = bounds & #xy . _y +~ i * rowHeight
        local (#widgetTree %~ WidgetTree.mapTree (#bounds %~ offsetRow)) $ do
          Updating.childBounds "name" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ show resource)
          Updating.childBounds "monthlyMined" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ printf "%.0f t" monthlyMined)
          Updating.childBounds "mineable" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ printf "%.0f t" available)
          Updating.childBounds "accessibility" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ printf "%.0f%%" (100 * accessibility))
          Updating.childBounds "priority" $ \bounds ->
            Widget.label (bounds ^. #xy) (fromString $ show priority)

          increasePriority <- Updating.childBounds "increasePriority" $ \bounds ->
            Widget.button bounds "+"
              <&> whenAlt (ChangePriority resource 1)

          decreasePriority <- Updating.childBounds "decreasePriority" $ \bounds ->
            Widget.button bounds "-"
              <&> whenAlt (ChangePriority resource (-1))

          pure (increasePriority <|> decreasePriority)

    actions
      & catMaybes
      & listToMaybe
      & pure
