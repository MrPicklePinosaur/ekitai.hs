import System.Environment

import qualified Data.Vector as V

import Parse
import Sim

main = do
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    putStrLn fname
    return 0

