module ScoreWriter where

import Prelude ( writeFile, IO, Show (show), (++), readFile, appendFile )
import System.IO ( )
import Types ( Score )

writeScore :: Score -> IO ()
writeScore s = do
    appendFile "app/Scores.txt" ("Score: " ++ show s ++ "\n")