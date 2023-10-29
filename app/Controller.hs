-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Types as T
    ( initialState,
      nO_SECS_BETWEEN_CYCLES,
      GameState(infoToShow, elapsedTime),
      InfoToShow(ShowABoard, ShowAChar),
      Orientation(Right, Up, Left, Down),
      emptyBoard )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char), Event(EventKey) )
import System.Random ()
import LevelLoader ()
import PacMan ( movePacManOrientation )

-- -- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gs =return gs
-- --    = let elapsedTime gs = elapsedTime gs + secs
-- --         do gs <- findAllTargetField gs
-- --            gs <- moveAllGhosts gs
-- --      return $ gs 

step :: Float -> GameState -> IO GameState
step secs gs
  | elapsedTime gs + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do return $ initialState
  | otherwise
  = -- Just update the elapsed time
    return $ gs { elapsedTime = elapsedTime gs + secs }
      

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)
 
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'c') _ _ _) gs = gs { infoToShow = ShowAChar 'c' }
inputKey (EventKey (Char 'b') _ _ _) gs = gs { infoToShow = ShowABoard emptyBoard }
inputKey (EventKey (Char 'w') _ _ _) gs = movePacManOrientation gs T.Up
inputKey (EventKey (Char 'a') _ _ _) gs = movePacManOrientation gs T.Left
inputKey (EventKey (Char 's') _ _ _) gs = movePacManOrientation gs T.Down
inputKey (EventKey (Char 'd') _ _ _) gs = movePacManOrientation gs T.Right
inputKey _ gs = gs -- Otherwise keep the same

--Volgorde wordt:
--1. Move PacMan
--2. Check dead?
--3. Update Field if not empty
--4. Update Score
--5. Find Ghost Target Fields
--6. Move Ghosts




-- step :: Float -> GameState -> IO GameState
-- step secs gs
--   | elapsedTime gs + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
    -- do randomNumber <- randomIO
    --    let newNumber = abs randomNumber `mod` 10
    --    return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
    -- return $ gs { elapsedTime = elapsedTime gs + secs }
-- 
