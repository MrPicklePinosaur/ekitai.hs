import System.Environment
import System.IO

import qualified Data.Vector as V
import qualified Brick as B

import Parse
import Sim
import Render

-- ui :: B.Widget ()
-- ui = B.str "hello" <+> B.str "World"

hGetLines :: Handle -> IO [String]
hGetLines h = do
    line <- hGetLine h
    isEof <- hIsEOF h
    if isEof then return [line]
    else do
        lines <- hGetLines h
        return (line:lines)

main = do
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    handle <- openFile fname ReadMode
    contents <- hGetLines handle
    hClose handle
    -- putStrLn $ show $ stringToSim contents
    initialState <- buildInitialState $ stringToSim contents
    endState <- B.defaultMain ekitaiApp initialState
    print endState
    return 0

-- main :: IO ()
-- main = do
    -- return 0

