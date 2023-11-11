{-# language CPP #-}

module PacMan where
import Types as T
    ( Board,
      Field(..),
      FieldType(Empty, Pellet, Cherry, PowerUp),
      GameState(GameState, board, elapsedTime, pacMan, score, ghostMode, ghostRed, ghostPink, ghostCyan, ghostOrange, infoToShow),
      GhostMode(Frightened),
      IsWall,
      Location(..),
      LocationCord,
      Orientation(..),
      PacMan(..),
      PacManLocation,
      Score(Score),
      ElapsedTime,
      NewOrientation,
      Size, Ghost (ghostLocation, Ghost, ghostStartLocation, ghostColor, ghostBaseColor), GhostLocation, Lives (Lives), PacManAnimation (..), pacManAnimationSpeed, pacManMouthSize, InfoToShow (ShowAChar), GhostColorTo (Dark) )
import Board
    ( locationToField,
      isWall,
      changeFieldType, findFieldCordAhead )
import Entity ( moveEntity, getCornerBoundaryLocations, boundaryCheck, snapToCenter )
import Ghost (changeAllGhostColor)
import Data.Maybe (mapMaybe)

-- Moves pac-man when given the gamestate, also checks for walls
movePacMan :: GameState -> PacManLocation
movePacMan gs@(GameState { elapsedTime = t
                         , board       = b
                         , pacMan      = p@(PacMan
                            { pacManLocation = l@(Location cords o)
                            , pacManFutureOrientation = fo
                            , pacManSpeed    = speed
                            , pacManSize     = size
                            })
                         }) 
    | o == fo && boundaryCheck b cords o size = snapToCenter l size 
    | o == fo                                 = moveEntity l speed t size
    | not (boundaryCheck b cords fo size)     = moveEntity (Location cords fo) speed t size
    | not (boundaryCheck b cords o size)      = moveEntity l speed t size
    | otherwise                               = l

interact :: Location -> GameState -> GameState
interact l gs@(GameState { board = b}) = let f = locationToField l b
                                         in case f of
                                            Just f -> handleField f gs
                                            Nothing -> gs

-- Reacts accordingly to the important field types pac-man could be on
handleField :: Field -> GameState -> GameState
handleField f@(MkField _ Pellet) gs@(GameState 
    { score = (Score s)
    , board = b
    }) = gs 
        { score = Score (s+1)    
        , board = changeFieldType b f Empty 
        }

handleField f@(MkField _ Cherry) gs@(GameState 
    { score = (Score s)
    , board = b
    }) = gs 
        { score = Score (s+5)
        , board = changeFieldType b f Empty 
        }

handleField f@(MkField _ PowerUp) gs@(GameState { board = b }) = changeAllGhostColor gs 
    { ghostMode = Frightened
    , board = changeFieldType b f Empty 
    } Dark

handleField f@(MkField _ _) gs = gs

-- Changes the gamestate depending on whether or not pac-man is in the same field as a ghost and the ghost mode, 
-- if they are and the ghost mode is frightened, reset the ghosts to their start position
enemyCollision :: GameState -> GameState
enemyCollision gs@(GameState 
    { board       = b
    , pacMan      = (PacMan 
        { lives               = Lives lvs
        , pacManLocation      = pacl
        , pacManStartLocation = pacsl
        })
    , ghostRed    = ghostR
    , ghostPink   = ghostP
    , ghostCyan   = ghostC
    , ghostOrange = ghostO
    , ghostMode   = Frightened
    }) = let pacf = locationToField pacl b
         in maybe gs (\pacf -> gs
         { ghostRed    = maybe ghostR killGhost (isGhostInSameField pacf ghostR b)
         , ghostPink   = maybe ghostP killGhost (isGhostInSameField pacf ghostP b)
         , ghostCyan   = maybe ghostC killGhost (isGhostInSameField pacf ghostC b)
         , ghostOrange = maybe ghostO killGhost (isGhostInSameField pacf ghostO b)  
         }) pacf

-- if pac-man and a ghost are in the same field and ghost mode is not frightened, then kill pac-man and reset all ghosts
enemyCollision gs@(GameState 
    { board       = b
    , pacMan      = (PacMan 
        { lives               = Lives lvs
        , pacManLocation      = pacl
        , pacManStartLocation = pacsl
        })
    , ghostRed    = (Ghost { ghostStartLocation = redsl })
    , ghostPink   = (Ghost { ghostStartLocation = pinksl })
    , ghostCyan   = (Ghost { ghostStartLocation = cyansl })
    , ghostOrange = (Ghost { ghostStartLocation = orangesl })
   , ghostMode   = _
    }) = let pacf = locationToField pacl b
         in maybe gs (checkCollidedGhosts gs . getCollidingEnemies gs) pacf

-- checks the list of ghosts given (this is the list of colliding ghosts with pac-man) and kills pac-man if it is not empty
checkCollidedGhosts :: GameState -> [Ghost] -> GameState 
checkCollidedGhosts gs []  = gs
checkCollidedGhosts gs _   = killPacMan gs

-- kills pac-man and resets the ghosts, resulting in pac-man losing a life
killPacMan :: GameState -> GameState
killPacMan gs@(GameState 
    { pacMan      = (PacMan 
        { lives               = Lives lvs
        , pacManStartLocation = pacsl
        })
    , ghostRed    = (Ghost { ghostStartLocation = redsl })
    , ghostPink   = (Ghost { ghostStartLocation = pinksl })
    , ghostCyan   = (Ghost { ghostStartLocation = cyansl })
    , ghostOrange = (Ghost { ghostStartLocation = orangesl })
    }) = gs
        { pacMan      = (pacMan gs) 
            { lives          = Lives (lvs - 1) 
            , pacManLocation = pacsl
            } 
        , ghostRed    = (ghostRed gs)    { ghostLocation = redsl }
        , ghostPink   = (ghostPink gs)   { ghostLocation = pinksl }
        , ghostCyan   = (ghostCyan gs)   { ghostLocation = cyansl }
        , ghostOrange = (ghostOrange gs) { ghostLocation = orangesl }
        }

-- kill a ghost, resets their position and color
killGhost :: Ghost -> Ghost
killGhost g@(Ghost 
    { ghostStartLocation = gsl 
    , ghostBaseColor     = gbc
    }) = g
        { ghostLocation = gsl 
        , ghostColor    = gbc
        }

-- goes through the list of ghosts and checks them one by one with pac-man and if they collide it puts them in a list
getCollidingEnemies :: GameState -> Field -> [Ghost]
getCollidingEnemies (GameState 
    { board       = b
    , ghostRed    = ghostR
    , ghostPink   = ghostP
    , ghostCyan   = ghostC
    , ghostOrange = ghostO
    }) pacf = mapMaybe (\g -> isGhostInSameField pacf g b) [ghostR,ghostP,ghostC,ghostO]

-- takes pac-mans field and a ghosts location then checks whether or not they are in the same field and returns the ghost if it is
isGhostInSameField :: Field -> Ghost -> Board -> Maybe Ghost
isGhostInSameField pacf g@(Ghost { ghostLocation = ghostl }) b = let ghostf = locationToField ghostl b
                                 in case ghostf of
                                    Just ghostf -> if ghostf == pacf then Just g else Nothing 
                                    Nothing     -> Nothing

-- checks if pac-man is dead or not, ends the game if he is
deathCheck :: GameState -> GameState
deathCheck gs@(GameState { pacMan = (PacMan { lives = l }) })
    | l == Lives 0    = gs { infoToShow = ShowAChar 'L' }
    | otherwise       = gs

-- pac-mans animation code, returns different states of animation for pac-man
pacManWakkaWakka :: GameState -> GameState
pacManWakkaWakka gs@(GameState { pacMan = 
                 p@(PacMan { pacManAnimation = Opening
                           , pacManPictureValues = (ma, pa, r, t) } ) }) 
                 = let (nma, npa, nr, nt) = (ma - pacManAnimationSpeed * elapsedTime gs, pa + pacManAnimationSpeed * elapsedTime gs, fromIntegral (pacManSize p) / 4, fromIntegral (pacManSize p) / 2)
                       na | npa >= pacManMouthSize = Closing
                          | otherwise              = Opening
                   in gs {pacMan = p { pacManPictureValues = (nma, npa, nr, nt)
                                     , pacManAnimation = na}}
pacManWakkaWakka gs@(GameState { pacMan = 
                 p@(PacMan { pacManAnimation = Closing
                           , pacManPictureValues = (ma, pa, r, t) } ) }) 
                 = let (nma, npa, nr, nt) = (ma + pacManAnimationSpeed * elapsedTime gs, pa - pacManAnimationSpeed * elapsedTime gs, fromIntegral (pacManSize p) / 4, fromIntegral (pacManSize p) / 2)
                       na | npa <= 0               = Opening
                          | otherwise              = Closing
                   in gs {pacMan = p { pacManPictureValues = (nma, npa, nr, nt)
                                     , pacManAnimation = na}}

