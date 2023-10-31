module Main where

--import Controller
import Types ( initialState, windowSize )
import Draw ( draw )
import Controller ( step, input )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
import LevelLoader (loadLevel)
-- 
main :: IO ()
main = do b <- loadLevel "app\\Level1.txt"
          playIO (InWindow "Counter" windowSize (0, 0)) -- Or FullScreen
           black            -- Background color
           60               -- Frames per second
           (initialState b)   -- Initial state
           draw             -- View function
           input            -- Event function
           step             -- Step function