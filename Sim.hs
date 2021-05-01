module Sim ( initSimSpace, testSim, physStep ) where

import           Data.Vector ((!), (//))
import qualified Data.Vector as V

-- Simulation simSpace simW simH
data Simulation = Simulation (V.Vector ChunkData) Int Int deriving (Show)

data ChunkType = Empty
    | Water 
    | Wall deriving (Show)

data ChunkData = ChunkData ChunkType deriving (Show)

-- vec accessors
veccGet v w i = v ! i
simGet (Simulation s w _) i = veccGet s w i

veccSet v w c i = v // [(i,c)]
simSet (Simulation s w h) c i = Simulation (veccSet s w c i) w h

initSimSpace x y = Simulation (V.replicate (y*x) (ChunkData Empty)) x y

testSim = simSet (initSimSpace 10 10) (ChunkData Water) 5

physStep sim@(Simulation _ w h) = _physStep 0 (initSimSpace w h) sim
_physStep i acc sim@(Simulation s w h) =
    if i >= w*h then acc
    else _physStep (i+1) next sim
    where
        getChunkData (ChunkData c) = c
        next = case getChunkData $ simGet sim i of
            Water -> sim
            _ -> sim

-- initGaussSeidel s =
--     where h = length s
--           w = length (s V.! 0)

-- gaussSeidel
