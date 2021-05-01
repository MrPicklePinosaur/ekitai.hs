module Render ( buildInitialState, ekitaiApp ) where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events

type ResourceName = String

data EkitaiState =
    EkitaiState deriving (Show)

ekitaiApp :: App EkitaiState e ResourceName
ekitaiApp = App
    { appDraw = drawEkitai
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEkitaiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO EkitaiState
buildInitialState = pure EkitaiState

drawEkitai :: EkitaiState -> [Widget ResourceName]
drawEkitai state = []

handleEkitaiEvent :: EkitaiState -> BrickEvent n e -> EventM n (Next EkitaiState)
handleEkitaiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                _ -> continue s
        _ -> continue s 

