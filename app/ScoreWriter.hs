module ScoreWriter where

import Prelude ( IO, Show (show), (++), appendFile )
import System.IO ( )
import Types ( Score )

-- writes to the scores txt if there is one and adds on to it
writeScore :: Score -> IO ()
writeScore s = do appendFile "app/Scores.txt" ("Score: " ++ show s ++ "\n")