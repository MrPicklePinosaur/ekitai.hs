import System.Environment
import System.IO

import qualified Data.Vector as V
import qualified Brick as B

import Parse
import Sim
import Render

-- ui :: B.Widget ()
-- ui = B.str "hello" <+> B.str "World"

main = do
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    handle <- openFile fname ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
    initialState <- buildInitialState $ stringToSim 10 10 contents
    endState <- B.defaultMain ekitaiApp initialState
    print endState

-- main :: IO ()
-- main = do
    -- return 0

