module Moe.Log where

import RIO
import RIO.Orphans ()

createLogFunc :: IO (LogFunc, IO ())
createLogFunc = do
    logOptions <- logOptionsHandle stdout True
    newLogFunc logOptions

destroyLogFunc :: (LogFunc, IO ()) -> IO ()
destroyLogFunc = snd
