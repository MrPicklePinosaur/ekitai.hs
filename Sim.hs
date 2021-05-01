module Sim ( initSimSpace, testSim, physStep, updateWaterChunk, validDirects ) where

import Debug.Trace
import           Data.Vector ((!), (//))
import qualified Data.Vector as V

data Simulation = Simulation
    { simSpace          :: V.Vector ChunkData
    , simW              :: Int
    , simH              :: Int
    } deriving (Show)

data ChunkType = Air
    | Water 
    | Wall deriving (Show, Enum)

data ChunkData = ChunkData 
    { chunkType         :: ChunkType
    } deriving (Show)

-- vec accessors
simGet Simulation{simSpace=s,simW=w} x y = s ! (y*w+x)
simSet sim@Simulation{simSpace=s,simW=w,simH=h} c x y = sim { simSpace = (s // [(y*w+x,c)]) }
simGetChunkType sim x y = chunkType $ simGet sim x y

initSimSpace x y = Simulation
    { simSpace = V.replicate (y*x) ChunkData { chunkType=Air }
    , simW = x
    , simH = y
    }

testSim = simSet (initSimSpace 3 3) (ChunkData Water) 1 1

physStep sim@Simulation{simW=w,simH=h} = _physStep [(x, y) | x <- [0..w-1], y <- [0..h-1]] sim sim
-- _physStep grid acc sim@Simulation{simSpace=s,simW=w,simH=h} | trace ("" ++ show acc) False = undefined
_physStep grid acc sim@Simulation{simSpace=s,simW=w,simH=h} =
    if null grid then acc
    else _physStep (tail grid) next sim
    where x = fst $ head grid
          y = snd $ head grid
          valid = validDirects x y w h
          next = case simGetChunkType sim x y of
              Water -> updateWaterChunk x y valid acc
              _ -> acc

-- takes in list of valid chunks, as well as the sim to modify
updateWaterChunk x y valid sim =
    -- if the chunk below is free, fall straight down
    if elem (0,-1) valid && fromEnum (simGetChunkType sim x (y-1)) == fromEnum Air
        then
            let moved = simSet sim ChunkData { chunkType=Air } x y
            in simSet moved ChunkData { chunkType=Water } x (y-1)
    else if elem (-1,-1) valid && fromEnum (simGetChunkType sim (x-1) (y-1)) == fromEnum Air
        then
            let moved = simSet sim ChunkData { chunkType=Air } x y
            in simSet moved ChunkData { chunkType=Water } (x-1) (y-1)
    else if elem (1,-1) valid && fromEnum (simGetChunkType sim (x+1) (y-1)) == fromEnum Air
        then
            let moved = simSet sim ChunkData { chunkType=Air } x y
            in simSet moved ChunkData { chunkType=Water } (x+1) (y-1)
    -- stay put
    else sim

-- gets chunks around a given chunk that are inside grid
validDirects x y w h = filter
    (\q -> 0 <= (fst q) && (fst q) < w && (snd q) <= 0 && (snd q) < h)
    [(a-x,b-y) | a <- [x-1..x+1], b <- [y-1..y+1], not (a==x && b==y)]

