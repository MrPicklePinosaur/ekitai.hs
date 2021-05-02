import System.Environment
import System.IO

import qualified Data.Vector as V

import Parse
import Sim
import Render

hGetLines :: Handle -> IO [String]
hGetLines h = do
    line <- hGetLine h
    isEof <- hIsEOF h
    if isEof then return [line]
    else do
        lines <- hGetLines h
        return (line:lines)

main = do
    -- handle file stuff
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    handle <- openFile fname ReadMode
    contents <- hGetLines handle
    hClose handle
    -- start brick
    ekitaiMain $ stringToSim contents
    return 0

