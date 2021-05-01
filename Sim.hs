module Sim (
    Simulation, simSpace,
    ChunkType, ChunkData, chunkType,
    initSimSpace, testSim, physStep, validDirects,
    simToString 
) where

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

testSim = let a = simSet (initSimSpace 10 10) (ChunkData Water) 5 5 
          in simSet a (ChunkData Wall) 5 4

physStep sim@Simulation{simW=w,simH=h} = _physStep [(x, y) | x <- [0..w-1], y <- [0..h-1]] sim sim
-- _physStep grid acc sim@Simulation{simSpace=s,simW=w,simH=h} | trace ("" ++ show h) False = undefined
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
-- updateWaterChunk x y valid sim | trace ("x:" ++ show x ++ " y:" ++ show y ++ " " ++ show valid) False = undefined
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
-- validDirects x y w h | trace ("w:"++ show w ++ "h:" ++ show h) False = undefined
validDirects x y w h = filter
    -- (\q -> 0 <= (fst q)+x && (fst q)+x < w && (snd q)+y <= 0 && (snd q)+y < h)
    (\q -> 0 <= (fst q)+x && (fst q)+x < w && 0 <= (snd q)+y && (snd q)+y < h)
    [(a,b) | a <- [-1..1], b <- [-1..1], not (a==0 && b==0)]

simToString :: Simulation -> [Char]
simToString sim@Simulation{simW=w} = 
    let simStr = V.toList $ V.map chunkToChar $ simSpace sim
    in insert w '\n' simStr

-- maps each chunktype to an ascii character
chunkToChar :: ChunkData -> Char
chunkToChar c =
    case chunkType c of
        Water -> '~'
        Air -> '.'
        Wall -> '#'
        _ -> '?'

-- from https://stackoverflow.com/questions/12659562/insert-specific-element-y-after-every-n-elements-in-a-list
insert :: Int -> a -> [a] -> [a]
insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

