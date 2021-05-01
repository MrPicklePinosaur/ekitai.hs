import System.Environment

import qualified Data.Vector as V
import qualified Brick as B

import Parse
import Sim
import Render

-- ui :: B.Widget ()
-- ui = B.str "hello" <+> B.str "World"

-- main :: IO ()
main = do
    initialState <- buildInitialState
    endState <- B.defaultMain ekitaiApp initialState
    print endState
    -- argv <- getArgs
    -- (opts, fname) <- ekitaiOpts argv
    -- return 0

