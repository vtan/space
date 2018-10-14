module App.UI.ProductionWindow where

import App.Prelude

import qualified App.Common.UidMap as UidMap
import qualified App.Logic.Colony as Logic.Colony
import qualified App.Model.Installation as Installation
import qualified App.Update.Updating as Updating
import qualified App.Update.Widget as Widget
import qualified Data.Text as Text

import App.Common.Rect (Rect(..))
import App.Common.Uid (Uid)
import App.Common.Util (whenAlt)
import App.Model.Body (Body(..))
import App.Model.BuildingTask (BuildingTask(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Update.Updating (Updating)
import Data.String (fromString)
import Text.Read (readMaybe)

update :: GameState -> Updating GameState
update gs@GameState{ bodies, colonies } = do
  Updating.childLayout "productionWindow" $ do
    Updating.childBounds "decoration" $ \bounds ->
      Widget.window bounds 20 "Production"

    let bodiesWithColony = toList (UidMap.zip bodies colonies)
    (selectedBodyColony, _) <-
      Updating.childBounds "selectedBody" $ \bounds ->
        Widget.listBox
          bounds 20
          (view (_1 . #uid)) (view (_1 . #name))
          #selectedBody
          bodiesWithColony

    case selectedBodyColony of
      Just (body, colony@Colony{ installations }) ->
        Updating.childLayout "rightPanel" $ do
          Updating.childLayout "installations" $ do
            let mineQty = installations ^. at Installation.Mine . non 0
            Updating.childBounds "mineCountLabel" $ \bounds ->
              Widget.label (bounds ^. #xy) "Mines"
            Updating.childBounds "mineCount" $ \bounds ->
              Widget.label (bounds ^. #xy) (fromString (show mineQty ++ " t"))

          Updating.childLayout "mining" $ do
            Updating.childBounds "header" $ \bounds ->
              Widget.label (bounds ^. #xy) "Mining"
          pure gs

      Nothing -> pure gs
