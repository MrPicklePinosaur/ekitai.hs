module Sim ( initSimSpace, testSim, physStep ) where

import           Data.Vector ((!), (//))
import qualified Data.Vector as V

-- Simulation simSpace simW simH
data Simulation = Simulation
    { simSpace          :: V.Vector ChunkData
    , simW              :: Int
    , simH              :: Int
    } deriving (Show)

data ChunkType = Empty
    | Water 
    | Wall deriving (Show)

data ChunkData = ChunkData 
    { chunkType         :: ChunkType
    } deriving (Show)

-- vec accessors
simGet Simulation{simSpace=s,simW=w} x y = s ! (y*w+x)
simSet sim@Simulation{simSpace=s,simW=w,simH=h} c x y = sim { simSpace = (s // [(y*w+x,c)]) }

initSimSpace x y = Simulation
    { simSpace = V.replicate (y*x) ChunkData { chunkType=Empty }
    , simW = x
    , simH = y
    }

testSim = simSet (initSimSpace 10 10) (ChunkData Water) 5 0

physStep sim@Simulation{simW=w,simH=h} = _physStep [(x, y) | x <- [0..w-1], y <- [0..h-1]] (initSimSpace w h) sim
_physStep grid acc sim@Simulation{simSpace=s,simW=w,simH=h} =
    if null grid then acc
    else _physStep (tail grid) next sim
    where x = fst $ head grid
          y = snd $ head grid
          next = sim

-- takes in list of valid chunks, as well as the sim to modify
-- updateWaterChunk valid sim =
    

-- gets chunks around a given chunk that are inside grid
validDirects x y w h = filter
    (\q -> 0 <= (fst q) && (fst q) < w && (snd q) <= 0 && (snd q) < h)
    [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not (a==x && b==y)]

