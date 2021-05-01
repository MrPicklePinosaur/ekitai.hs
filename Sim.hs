module Sim ( ChunkData, initSimSpace, defaultChunkData ) where

import qualified Data.Vector as V

data ChunkData = ChunkData
    { velocity          :: (Float, Float)
    , density           :: Float
    } deriving (Show)

defaultChunkData = ChunkData
    { velocity          = (0,0)
    , density           = 0
    }

initSimSpace :: Int -> Int -> V.Vector ChunkData
initSimSpace x y = V.replicate (y*x) defaultChunkData

-- gaussSeidel
