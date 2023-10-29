-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import LevelLoader
import PacMan

-- -- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gstate =return gstate
-- --    = let elapsedTime gstate = elapsedTime gstate + secs
-- --         do gstate <- findAllTargetField gstate
-- --            gstate <- moveAllGhosts gstate
-- --      return $ gstate 

step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do return $ initialState
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }
      

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
 
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'c') _ _ _) gstate = gstate { infoToShow = ShowAChar 'c' }
inputKey (EventKey (Char 'w') _ _ _) gstate = gstate { pacmanLocation = movePacMan board pacmanLocation Up pacmanSpeed elapsedTime }
inputKey (EventKey (Char 'a') _ _ _) gstate = gstate { pacmanLocation = movePacMan board pacmanLocation Left pacmanSpeed elapsedTime }
inputKey (EventKey (Char 's') _ _ _) gstate = gstate { pacmanLocation = movePacMan board pacmanLocation Down pacmanSpeed elapsedTime }
inputKey (EventKey (Char 'd') _ _ _) gstate = gstate { pacmanLocation = movePacMan board pacmanLocation Right pacmanSpeed elapsedTime }
inputKey _ gstate = gstate -- Otherwise keep the same

--Volgorde wordt:
--1. Move PacMan
--2. Check dead?
--3. Update Field if not empty
--4. Update Score
--5. Find Ghost Target Fields
--6. Move Ghosts




-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
    -- do randomNumber <- randomIO
    --    let newNumber = abs randomNumber `mod` 10
    --    return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
    -- return $ gstate { elapsedTime = elapsedTime gstate + secs }
-- 
