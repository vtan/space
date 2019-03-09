module App.View.ColonyWindow
  ( update )
where

import App.Prelude

import qualified App.Common.Print as Print
import qualified App.Logic.Colony as ColonyLogic
import qualified App.Model.Installation as Installation
import qualified App.Model.Resource as Resource
import qualified App.Model.Ship as Ship
import qualified App.UI2.UI as UI
import qualified App.UI2.Widget as Widget

import App.Model.Body (Body(..))
import App.Model.Colony (Colony(..))
import App.Model.GameState (GameState(..))
import App.Model.Installation (Installation)
import App.Model.Mineral (Mineral(..))
import App.Model.Resource (Resource)
import App.Model.ShipBuildingTask (ShipBuildingTask(..))
import App.Update.Updating (Updating)

update :: GameState -> Updating GameState
update gs@GameState{ bodies, bodyMinerals, colonies } =
  UI.positioned (V2 180 20) . UI.sized (V2 150 150) $
    Widget.window Widget.Window{ titleHeight = 5, title = "Colonies" } $
      UI.group UI.Horizontal $ do
        selectedBody <- bodyList (toList bodies)
        case selectedBody of
          Just Body{ bodyId } ->
            UI.group UI.Vertical . UI.sized (V2 30 5) $ do
              mineablePanel (bodyMinerals ^. at bodyId . _Just)
              case colonies ^. at bodyId of
                Just colony -> do
                  stockpilePanel colony
                  pure gs
                Nothing -> do
                  found <- Widget.button "Found colony"
                  pure $
                    if found
                    then ColonyLogic.foundColony bodyId gs
                    else gs
          Nothing -> pure gs

bodyList :: [Body] -> Updating (Maybe Body)
bodyList bodies =
  fmap fst $
    UI.width 35 $
      Widget.listBox
        Widget.ListBox
          { itemHeight = 5
          , scrollBarSize = V2 1 2
          , toIx = view #bodyId
          , toText = view #name
          }
        (#ui . #selectedBody)
        (toList bodies)

mineablePanel :: HashMap Resource Mineral -> Updating ()
mineablePanel minerals = do
  UI.width 40 (Widget.label' "Mineable resources")
  UI.group' . UI.padded 0 $ do
    when ((not . null) minerals) $
      UI.group UI.Horizontal $ do
        Widget.label' "Mineral"
        Widget.label' "Available"
        Widget.label' "Mining speed"
    ifor_ minerals $ \resource Mineral{ available, accessibility } ->
      UI.group UI.Horizontal $ do
        Widget.label (Resource.print resource)
        Widget.label (Print.float0 available <> " t")
        Widget.label (Print.float0 (100 * accessibility) <> "%")

stockpilePanel :: Colony -> Updating ()
stockpilePanel Colony{ stockpile } = do
  Widget.label' "Stockpile"
  UI.padded 0 $ do
    when ((not . null) stockpile) $
      UI.group UI.Horizontal $ do
        Widget.label' "Mineral"
        Widget.label' "In stockpile"
    ifor_ stockpile $ \resource quantity ->
      UI.group UI.Horizontal $ do
        Widget.label (Resource.print resource)
        Widget.label (Print.float0 quantity <> " t")
