module Main where

--import Controller
import Types ( initialState, windowSize )
import Draw ( draw )
import Controller ( step, input )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
-- 
main :: IO ()
main = playIO (InWindow "Counter" windowSize (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              draw             -- View function
              input            -- Event function
              step             -- Step function