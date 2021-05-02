module Parse ( ekitaiOpts, optTimeStep, hGetLines ) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import Data.Either

data Options = Options
    { optHelp       :: Bool
    , optColor      :: Bool
    , optTimeStep   :: Int
    }

defaultOptions = Options
    { optHelp       = False
    , optColor      = False
    , optTimeStep   = 50000
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "display's this message"
    , Option ['c'] ["color", "colour"]
        (NoArg (\opts -> opts { optColor = True }))
        "enables color"
    , Option ['t'] ["timestep"]
        (ReqArg (\t opts -> opts { optTimeStep = read t :: Int }) "timestep")
        "sets the simulation time step"
    ]

ekitaiOpts :: [String] -> IO (Options, String)
ekitaiOpts argv =
    case getOpt RequireOrder options argv of
        (o, [n], []) -> return (foldl (flip id) defaultOptions o, n)
        (o, _, [])   -> ioError $ userError $ "missing input file"
        (_, _, errs) -> ioError $ userError $ concat errs 

-- reads in file by lines
hGetLines :: Handle -> IO [String]
hGetLines h = do
    line <- hGetLine h
    isEof <- hIsEOF h
    if isEof then return [line]
    else do
        lines <- hGetLines h
        return (line:lines)

