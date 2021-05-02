import System.Environment
import System.IO

import qualified Data.Vector as V

import Parse
import Sim
import Render

main = do
    -- handle file stuff
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    handle <- openFile fname ReadMode
    contents <- hGetLines handle
    hClose handle
    -- start brick
    ekitaiMain (stringToSim contents) (optTimeStep opts)
    return 0

