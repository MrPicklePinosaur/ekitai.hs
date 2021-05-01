module Sim ( initSimSpace, testSim, physStep, validDirects ) where

import           Data.Vector ((!), (//))
import qualified Data.Vector as V

-- Simulation simSpace simW simH
data Simulation = Simulation (V.Vector ChunkData) Int Int deriving (Show)

data ChunkType = Empty
    | Water 
    | Wall deriving (Show)

data ChunkData = ChunkData ChunkType deriving (Show)

-- vec accessors
simGet (Simulation s w _) x y = s ! (y*w+x)
simSet (Simulation s w h) c x y = Simulation (s // [(y*w+x,c)]) w h

initSimSpace x y = Simulation (V.replicate (y*x) (ChunkData Empty)) x y

testSim = simSet (initSimSpace 10 10) (ChunkData Water) 5 0

physStep sim@(Simulation _ w h) = _physStep [(x, y) | x <- [0..w-1], y <- [0..h-1]] (initSimSpace w h) sim
_physStep grid acc sim@(Simulation s w h) =
    if null grid then acc
    else _physStep (tail grid) next sim
    where x = fst $ head grid
          y = snd $ head grid
          next = sim

-- gets chunks around a given chunk that are inside grid
validDirects x y w h = filter
    (\q -> 0 <= (fst q) && (fst q) < w && (snd q) <= 0 && (snd q) < h)
    [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not (a==x && b==y)]

