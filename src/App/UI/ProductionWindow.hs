module App.UI.ProductionWindow
  ( update )
where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Dimension.Time as Time
import qualified App.Logic.Mining as Logic.Mining
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
  Updating.useWidget "productionWindow" $ do
    Updating.widget "decoration" $
      Widget.window 20 "Production"

    let bodiesWithColony = toList (UidMap.zip bodies colonies)
    (selectedColony, _) <-
      Updating.widget "selectedBody"
        ( Widget.listBox
            20 (view (_1 . #uid)) (view (_1 . #name))
            #selectedBody
            bodiesWithColony
        ) <&> (_1 . mapped %~ snd)

    case selectedColony of
      Just colony@Colony{ bodyUid, installations } -> do
        let minerals = bodyMinerals ^. at bodyUid . non mempty
        Updating.useWidget "rightPanel" $ do
          Updating.useWidget "installations" $ do
            let mineQty = installations ^. at Installation.Mine . non 0
            Updating.widget "mineCountLabel" $
              Widget.label "Mines"
            Updating.widget "mineCount" $
              Widget.label (fromString (show mineQty))

          miningAction <- Updating.useWidget "mining" $
            miningPanel colony minerals

          pure $ case miningAction of
            Just (ChangePriority resource diff) ->
              Logic.Mining.changeMiningPriority resource diff colony gs
            Nothing -> gs

      Nothing -> pure gs

miningPanel :: Colony -> HashMap Resource Mineral -> Updating (Maybe Action)
miningPanel colony@Colony{ miningPriorities, stockpile } minerals = do
  Updating.useWidget "headingRow" $ do
    let totalMonthlyMined = Time.daysInMonth * Logic.Mining.dailyMinedAtFullAccessibility colony
    Updating.widget "title" $ Widget.label "Mining"
    Updating.widget "totalMined" $
      Widget.label (fromString $ printf "Mined / mo at 100%% acc.: %.0f t" totalMonthlyMined)
    Updating.thisWidget Widget.bottomLine

  Updating.useWidget "headerRow" $ do
    Updating.widget "monthlyMined" $ Widget.label "Mined / mo"
    Updating.widget "priority" $ Widget.label "Priority"
    Updating.widget "mineable" $ Widget.label "Mineable"
    Updating.widget "accessibility" $ Widget.label "Accessibility"
    Updating.widget "stockpile" $ Widget.label "Stockpile"

  Updating.useWidget "mineralRows" $ do
    let allMonthlyMined = Time.daysInMonth *^ Logic.Mining.dailyMined colony minerals
    actions <- ifor Resource.minerals $ \i resource -> do
      let Mineral{ available, accessibility } = minerals ^. at resource . Mineral.nonEmpty
          monthlyMined = allMonthlyMined ^. at resource . non 0
          priority = miningPriorities ^. at resource . non 0
          inStockpile = stockpile ^. at resource . non 0
      Updating.useWidget "row" $ do
        rowHeight <- view (#widgetTree . #bounds . #wh . _y)
        let offsetRow bounds = bounds & #xy . _y +~ i * rowHeight
        local (#widgetTree %~ WidgetTree.mapTree (#bounds %~ offsetRow)) $ do
          Updating.widget "name" $
            Widget.label (fromString $ show resource)
          Updating.widget "monthlyMined" $
            Widget.label (fromString $ printf "%.0f t" monthlyMined)
          Updating.widget "mineable" $
            Widget.label (fromString $ printf "%.0f t" available)
          Updating.widget "accessibility" $
            Widget.label (fromString $ printf "%.0f%%" (100 * accessibility))
          Updating.widget "stockpile" $
            Widget.label (fromString $ printf "%.0f t" inStockpile)
          Updating.widget "priority" $
            Widget.label (fromString $ show priority)

          increasePriority <- Updating.widget "increasePriority"
            (Widget.button "+")
            <&> whenAlt (ChangePriority resource 1)

          decreasePriority <- Updating.widget "decreasePriority"
            (Widget.button "-")
            <&> whenAlt (ChangePriority resource (-1))

          pure (increasePriority <|> decreasePriority)

    pure (asum actions)
