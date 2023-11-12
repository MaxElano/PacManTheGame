-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Types as T
    ( initialState,
      GameState(..),
      InfoToShow(..),
      GhostHouseStatus(..),
      TotalTime,
      Ghost(..),
      Orientation(Right, Up, Left, Down),
      PacMan (..), Location (Location), Field, ElapsedTime, Time )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char), Event(EventKey) )
import System.Random (RandomGen (genShortByteString))
import LevelLoader ()
import PacMan ( movePacMan, pacManWakkaWakka, handleField, enemyCollision, interact, deathCheck )
import Entity (moveEntity)
import Board (locationToField, lCordToFCord, setEndOfGame)
import Ghost (moveAllGhosts, findAllTargetFields, handleGhostTimers)
import qualified Graphics.Gloss.Interface.IO.Game as KeyState

step :: Float -> GameState -> IO GameState
step secs gs@(GameState 
    { paused = False , 
    finished = False
    }) = do update gs 
             { totalTime = totalTime gs + secs
             , elapsedTime = secs
             }

step _ gs = return gs
      
update :: GameState -> IO GameState
update gs@(GameState { pacMan = (PacMan { pacManLocation = l }) 
                     , board  = b
                     }) = 
                     do return 
                     . setEndOfGame
                     . handleTimers
                     . deathCheck 
                     . enemyCollision 
                     . pacManWakkaWakka 
                     . moveAllGhosts 
                     . findAllTargetFields
                     . PacMan.interact l $ gs { pacMan = (pacMan gs) { pacManLocation = movePacMan gs } }

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)
 
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'c') _ _ _) gs@(GameState { ghostRed = (Ghost { ghostMode = gm }) }) = gs { infoToShow = ShowAMode gm }
inputKey (EventKey (Char 'b') _ _ _) gs = gs { infoToShow = ShowPlayState }
inputKey (EventKey (Char 'w') _ _ _) gs = gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Up } }
inputKey (EventKey (Char 'a') _ _ _) gs = gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Left } }
inputKey (EventKey (Char 's') _ _ _) gs = gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Down } }
inputKey (EventKey (Char 'd') _ _ _) gs = gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Right } }
inputKey (EventKey (Char 'p') _ _ _) gs@(GameState { infoToShow = drawState
                                                   , paused = pauseState
                                                   , keyStatePaused = KeyState.Up }) = gs { infoToShow = changePausedState drawState, paused = not pauseState, keyStatePaused = KeyState.Down }
inputKey (EventKey (Char 'n') _ _ _) gs = gs { infoToShow = ShowANumber 2 }
inputKey _ gs = gs { keyStatePaused = KeyState.Up}


changePausedState :: InfoToShow -> InfoToShow
changePausedState ShowPlayState  = ShowPauseState
changePausedState ShowPauseState = ShowPlayState
changePausedState i              = i

handleTimers :: GameState -> GameState
handleTimers gs = gs { ghostRed    = handleGhostTimers (ghostRed gs) (elapsedTime gs)
                     , ghostOrange = handleGhostTimers (ghostOrange gs) (elapsedTime gs) 
                     , ghostPink   = handleGhostTimers (ghostPink gs) (elapsedTime gs) 
                     , ghostCyan   = handleGhostTimers (ghostCyan gs) (elapsedTime gs) 
                     }