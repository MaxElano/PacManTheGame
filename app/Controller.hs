-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Types as T
    ( initialState,
      GameState(..),
      InfoToShow(ShowABoard, ShowAChar, ShowPlayState, ShowANumber),
      Orientation(Right, Up, Left, Down),
      PacMan (..) )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char), Event(EventKey) )
import System.Random ()
import LevelLoader ()
import PacMan ( movePacManOrientation, pacManWakkaWakka )
import Entity (moveEntity)

-- -- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gs =return gs
-- --    = let elapsedTime gs = elapsedTime gs + secs
-- --         do gs <- findAllTargetField gs
-- --            gs <- moveAllGhosts gs
-- --      return $ gs 

step :: Float -> GameState -> IO GameState
step secs gs = do update gs { totalTime   = totalTime gs + secs
                            , elapsedTime = secs
                            }
      
update :: GameState -> IO GameState
update gs = do return $ pacManWakkaWakka gs { pacMan = updatePacMan gs }

updatePacMan :: GameState -> PacMan
updatePacMan gs@(GameState { elapsedTime = t
                           , pacMan      = p@(PacMan
                            { pacManLocation = l
                            , pacManSpeed    = s
                            })
                           }) = p{ pacManLocation = moveEntity l s t }

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)
 
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'c') _ _ _) gs = gs { infoToShow = ShowANumber (elapsedTime gs) }
inputKey (EventKey (Char 'p') _ _ _) gs = gs { infoToShow = ShowPlayState }
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
