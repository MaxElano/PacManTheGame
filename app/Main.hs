module Main where

--import Controller
import Types
import Draw
import Controller

import Graphics.Gloss.Interface.IO.Game
-- 
main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              draw             -- View function
              input            -- Event function
              step             -- Step function