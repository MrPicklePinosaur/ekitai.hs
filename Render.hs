module Render ( ekitaiMain ) where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.BChan (newBChan, writeBChan)
import Graphics.Vty

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import qualified Data.Vector as V
import Sim

type ResourceName = String

data EkitaiState = EkitaiState
    { ekitaiStateSim           :: Simulation
    } deriving (Show)

-- custom event
data Tick = Tick

ekitaiApp :: App EkitaiState Tick ResourceName
ekitaiApp = App
    { appDraw = drawEkitai
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEkitaiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

ekitaiMain sim timestep = do
    chan <- newBChan 10
    -- tick game
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay timestep
    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    initialVty <- buildVty
    initialState <- buildInitialState sim
    endState <- customMain initialVty buildVty (Just chan) ekitaiApp initialState
    return 0

buildInitialState :: Simulation -> IO EkitaiState
buildInitialState sim =
    pure EkitaiState
    { ekitaiStateSim = sim
    }

drawEkitai :: EkitaiState -> [Widget ResourceName]
drawEkitai state = [ vBox [str $ simToString $ ekitaiStateSim state] ]

handleEkitaiEvent :: EkitaiState -> BrickEvent n Tick -> EventM n (Next EkitaiState)
handleEkitaiEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handleEkitaiEvent s (AppEvent Tick) = continue s { ekitaiStateSim = physStep $ ekitaiStateSim s }
handleEkitaiEvent s _ = continue s

