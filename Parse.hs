module Parse ( ekitaiOpts, optTimeStep ) where

import System.Console.GetOpt
import System.Environment
import System.Exit
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

-- ++ usageInfo header options
--         where header = "Usage: ekitai [OPTIONS...] simfile"

        -- (o, _, [])   -> do
        --     opts <- (foldl (flip id) defaultOptions o)
        --     if optHelp then userError $ concat usageInfo "Usage: ekitai [OPTIONS...] simfile" options
        --     else ioError $ userError $ "missing input file"
