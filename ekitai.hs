import System.Console.GetOpt
import System.Environment
import System.Exit
import Data.Maybe
import Data.Either

data Flag 

data Options = Options
    { optHelp       :: Bool
    , optColor      :: Bool
    }

defaultOptions = Options
    { optHelp       = False
    , optColor      = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\ opts -> opts { optHelp = True }))
        "display's this message"
    , Option ['c'] ["color", "colour"]
        (NoArg (\ opts -> opts { optColor = True }))
        "enables color"
    ]

ekitaiOpts :: [String] -> Either String (Options, String)
ekitaiOpts argv =
    case getOpt RequireOrder options argv of
        (o, n, []) -> undefined
        (_, _, err) -> Left "invalid"

main = do
    argv <- getArgs
    return 0

