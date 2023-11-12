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
      PacMan (..), Location (Location), Field (MkField), ElapsedTime, Time, Lives (Lives), Finished, FieldType (..) )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, SpecialKey), Event(EventKey), SpecialKey (KeySpace) )
import System.Random (RandomGen (genShortByteString))
import LevelLoader ()
import PacMan ( movePacMan, pacManWakkaWakka, handleField, enemyCollision, interact, )
import Entity (moveEntity)
import Board (locationToField, lCordToFCord)
import Ghost (moveAllGhosts, findAllTargetFields, handleGhostTimers )
import qualified Graphics.Gloss.Interface.IO.Game as KeyState
import System.Exit
import ScoreWriter (writeScore)

step :: Float -> GameState -> IO GameState
step secs gs@(GameState { paused = False }) = do update gs 
                                                  { totalTime = totalTime gs + secs
                                                  , elapsedTime = secs
                                                  }
step _ gs = return gs
      
update :: GameState -> IO GameState
update gs@(GameState 
    { pacMan = (PacMan 
        { pacManLocation = l
        , lives = (Lives lvs) 
        }) 
    , board  = b
    , score  = s
    }) 
    | finished gs && lvs > 0 = do return gs { infoToShow = ShowWinState }
    | finished gs            = do return gs { infoToShow = ShowLostState }
    | otherwise              = do return
                               . handleTimers
                               . checkEndOfGame 
                               . enemyCollision 
                               . pacManWakkaWakka 
                               . moveAllGhosts 
                               . findAllTargetFields
                               . PacMan.interact l $ gs { pacMan = (pacMan gs) { pacManLocation = movePacMan gs } } 


-- handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (Char 'c') _ _ _) gs@(GameState { ghostRed = (Ghost { ghostMode = gm }) }) = return gs { infoToShow = ShowAMode gm }
input (EventKey (Char 'b') _ _ _) gs = return gs { infoToShow = ShowPlayState }
input (EventKey (Char 'w') _ _ _) gs = return gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Up } }
input (EventKey (Char 'a') _ _ _) gs = return gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Left } }
input (EventKey (Char 's') _ _ _) gs = return gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Down } }
input (EventKey (Char 'd') _ _ _) gs = return gs { pacMan = (pacMan gs) { pacManFutureOrientation = T.Right } }
input (EventKey (Char 'p') _ _ _) gs@(GameState { infoToShow = drawState
                                                   , paused = pauseState
                                                   , keyStatePaused = KeyState.Up }) = return gs { infoToShow = changePausedState drawState, paused = not pauseState, keyStatePaused = KeyState.Down }
input (EventKey (SpecialKey KeySpace) _ _ _) gs@(GameState 
    { infoToShow = ShowWinState
    , score = s 
    }) = do writeScore s
            exitSuccess 
input (EventKey (SpecialKey KeySpace) _ _ _) gs@(GameState 
    { infoToShow = ShowLostState
    , score = s 
    }) = do writeScore s
            exitSuccess 
             
input _ gs = return gs { keyStatePaused = KeyState.Up}

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

checkEndOfGame :: GameState -> GameState
checkEndOfGame gs@(GameState { pacMan = (PacMan { lives = Lives 0 }) }) = gs { finished = True }
checkEndOfGame gs@(GameState { board = b })                             = gs { finished = not $ foldr (\rs x -> x || foldr (\(MkField _ f) y -> y || case f of
    Pellet  -> True
    Cherry  -> True
    PowerUp -> True
    _       -> False) False rs) False b }