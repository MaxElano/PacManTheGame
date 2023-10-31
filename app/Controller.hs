-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Types as T
    ( initialState,
      GameState(..),
      InfoToShow(ShowABoard, ShowAChar, ShowPlayState, ShowANumber, ShowAPosition, ShowAnIntTuple),
      Orientation(Right, Up, Left, Down),
      PacMan (..), Location (Location), Field )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char), Event(EventKey) )
import System.Random (RandomGen (genShortByteString))
import LevelLoader ()
import PacMan ( changePacManOrientation, movePacMan, pacManWakkaWakka, handleField )
import Entity (moveEntity)
import Board (locationToField, lCordToFCord)

-- -- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gs =return gs
-- --    = let elapsedTime gs = elapsedTime gs + secs
-- --         do gs <- findAllTargetField gs
-- --            gs <- moveAllGhosts gs
-- --      return $ gs 

step :: Float -> GameState -> IO GameState
step secs gs = do update gs 
                   { totalTime = totalTime gs + secs
                   , elapsedTime = secs
                   }
      
update :: GameState -> IO GameState
update gs@(GameState { pacMan = (PacMan { pacManLocation = l }) 
                     , board  = b
                     }) = 
                     do return $ pacManWakkaWakka $ (Controller.interact l) gs { pacMan = (pacMan gs) { pacManLocation = movePacMan gs } }
--                     do return $ pacManWakkaWakka $ Controller.interact l gs { pacMan = (pacMan gs) { pacManLocation = movePacMan gs } }

interact :: Location -> GameState -> GameState
interact l gs@(GameState { board = b}) = let f = locationToField l b
                                         in case f of
                                            Just f -> handleField f gs
                                            Nothing -> gs

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)
 
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'c') _ _ _) gs@(GameState { pacMan = (PacMan { pacManLocation = l }) }) = gs { infoToShow = ShowAPosition l }
inputKey (EventKey (Char 'v') _ _ _) gs@(GameState { pacMan = (PacMan { pacManLocation = (Location cords _) }) }) = gs { infoToShow = ShowAnIntTuple (lCordToFCord cords) }
inputKey (EventKey (Char 'p') _ _ _) gs = gs { infoToShow = ShowPlayState }
inputKey (EventKey (Char 'w') _ _ _) gs = changePacManOrientation gs T.Up
inputKey (EventKey (Char 'a') _ _ _) gs = changePacManOrientation gs T.Left
inputKey (EventKey (Char 's') _ _ _) gs = changePacManOrientation gs T.Down
inputKey (EventKey (Char 'd') _ _ _) gs = changePacManOrientation gs T.Right
inputKey (EventKey (Char 'b') _ _ _) gs = gs { infoToShow = ShowABoard (board gs) }
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
