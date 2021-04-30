import System.Environment

import Parse

main = do
    argv <- getArgs
    (opts, fname) <- ekitaiOpts argv
    putStrLn fname
    return 0

