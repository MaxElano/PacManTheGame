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
      PacMan (..), Location (Location), Field (MkField), ElapsedTime, Time, Lives (Lives), Finished, FieldType (..), PacManAnimation (Dying) )
import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, SpecialKey), Event(EventKey), SpecialKey (KeySpace) )
import System.Random (RandomGen (genShortByteString))
import LevelLoader ()
import PacMan ( movePacMan, pacManWakkaWakka, handleField, enemyCollision, interact, )
import Entity (moveEntity)
import Board (locationToField, lCordToFCord)
import Ghost (moveAllGhosts, findAllTargetFields, handleGhostTimers, handleGhostsTimers )
import qualified Graphics.Gloss.Interface.IO.Game as KeyState
import System.Exit
import ScoreWriter (writeScore)

-- updates the time every frame except when the game is paused
step :: Float -> GameState -> IO GameState
step secs gs@(GameState { paused = False }) = do update gs 
                                                  { totalTime = totalTime gs + secs
                                                  , elapsedTime = secs
                                                  }
step _ gs = return gs
      
-- updates the game logic based on different situations
update :: GameState -> IO GameState
update gs@(GameState 
    { pacMan = (PacMan 
        { pacManLocation = l
        , lives = (Lives lvs) 
        , pacManAnimation = pa
        }) 
    , board  = b
    , score  = s
    }) 
    | finished gs && lvs > 0 = do return gs { infoToShow = ShowWinState }
    | finished gs            = do return gs { infoToShow = ShowLostState }
    | pa == Dying            = do return (pacManWakkaWakka gs)
    | otherwise              = do return
                               . handleGhostsTimers
                               . checkEndOfGame 
                               . enemyCollision 
                               . pacManWakkaWakka 
                               . moveAllGhosts 
                               . findAllTargetFields
                               . PacMan.interact l $ gs { pacMan = (pacMan gs) { pacManLocation = movePacMan gs } } 


-- handles user input
input :: Event -> GameState -> IO GameState
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

-- changes the state from paused to play and vice versa
changePausedState :: InfoToShow -> InfoToShow
changePausedState ShowPlayState  = ShowPauseState
changePausedState ShowPauseState = ShowPlayState
changePausedState i              = i

-- checks whehter or not the game is finished (when pacman has lost all his lives or when all the pickups are gone)
checkEndOfGame :: GameState -> GameState
checkEndOfGame gs@(GameState { pacMan = (PacMan { lives = Lives 0 }) }) = gs { finished = True }
checkEndOfGame gs@(GameState { board = b })                             = gs { finished = not $ foldr (\rs x -> x || foldr (\(MkField _ f) y -> y || case f of
    Pellet  -> True
    Cherry  -> True
    PowerUp -> True
    _       -> False) False rs) False b }