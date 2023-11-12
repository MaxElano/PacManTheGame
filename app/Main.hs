module Main where

--import Controller
import Types ( initialState, windowSize, Score (Score) )
import Draw ( draw )
import Controller ( step, input )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
import LevelLoader (loadLevel)
import Graphics.Gloss
import ScoreWriter (writeScore)
-- 
main :: IO ()
main = do putStrLn "Choose level by inserting one of the following chars: '1', '2', '3', 'c' (c stands for the custom level)"
          c <- getChar
          if c == '1'
            then do b <- loadLevel "app\\Level1.txt"
                    playIO (InWindow "Counter" windowSize (0, 0)) -- Or FullScreen
                     black            -- Background color
                     600              -- Frames per second
                     (initialState b) -- Initial state
                     draw             -- View function
                     input            -- Event function
                     step             -- Step function
              else do writeScore (Score 3)
--            else do display (InWindow "Counter" windowSize (0, 0)) red (scale 0.2 0.2 $ translate (-30) 0 $ color black (text "&&%&#$%&E#RR#OR&&%&$#%&"))

