{-# language CPP #-}

module PacMan where
import Types as T
    ( Board,
      Field(..),
      FieldType(Empty, Pellet, Cherry, PowerUp),
      GameState(GameState, board, elapsedTime, pacMan, score, ghostMode, ghostRed, ghostPink, ghostCyan, ghostOrange),
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
      Size, Ghost (ghostLocation, Ghost, ghostStartLocation), GhostLocation, Lives (Lives), PacManAnimation (..), pacManAnimationSpeed, pacManMouthSize )
import Board 
    ( locationToField, 
      wallCheck, 
      changeFieldType )
import Entity ( moveEntity )

-- Changes the orientation of pac-man given by wasd input
changePacManOrientation :: GameState -> NewOrientation -> GameState
changePacManOrientation gs@(GameState { pacMan = (PacMan { pacManLocation = (Location cords _) }) }) no = 
    gs { pacMan = (pacMan gs) { pacManLocation = Location cords no } }

-- Moves pac-man when given the gamestate, also checks for walls
movePacMan :: GameState -> PacManLocation
movePacMan gs@(GameState { elapsedTime = t
                         , board       = b
                         , pacMan      = p@(PacMan
                            { pacManLocation = l
                            , pacManSpeed    = s
                            })
                         }) = let isWall = boundaryCheck b p 
                              in if isWall then l else moveEntity l s t

-- Checks if the edge of pacman is in a wall or not in the new location
boundaryCheck :: Board -> PacMan -> IsWall
boundaryCheck b p@(PacMan 
    { pacManLocation = (Location cords o)
    , pacManSize     = size
    }) = let nf = locationToField (Location (getBoundaryLocation cords o size) Up) b
         in  maybe False wallCheck nf

-- Gives the coordinates of the edge of pacman in the new location
getBoundaryLocation :: LocationCord -> Orientation -> Size -> LocationCord
getBoundaryLocation (x,y) T.Up    size = (x, y + fromIntegral (size `div` 2))
getBoundaryLocation (x,y) T.Right size = (x    + fromIntegral (size `div` 2), y)
getBoundaryLocation (x,y) T.Down  size = (x, y - fromIntegral (size `div` 2))
getBoundaryLocation (x,y) T.Left  size = (x    - fromIntegral (size `div` 2), y)

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

handleField f@(MkField _ PowerUp) gs@(GameState { board = b }) = gs 
    { ghostMode = Frightened
    , board = changeFieldType b f Empty 
    }

handleField f@(MkField _ _) gs = gs

-- Changes the gamestate depending on whether or not pac-man is in the same field as a ghost, 
-- if they are, the game to the start position and pac-man loses a life
enemyCollision :: GameState -> PacManLocation -> GameState
enemyCollision gs@(GameState 
    { board       = b
    , pacMan      = (PacMan 
        { lives               = Lives lvs 
        , pacManStartLocation = pacsl
        })
    , ghostRed    = (Ghost { ghostStartLocation = redsl })
    , ghostPink   = (Ghost { ghostStartLocation = pinksl })
    , ghostCyan   = (Ghost { ghostStartLocation = cyansl })
    , ghostOrange = (Ghost { ghostStartLocation = orangesl })
    }) pacl = let pacf = locationToField pacl b
        in case pacf of
            Just pacf -> if checkEnemyCollision gs pacf b
                then gs
                    { pacMan      = (pacMan gs) 
                        { lives          = Lives (lvs - 1) 
                        , pacManLocation = pacsl       
                        } 
                    , ghostRed    = (ghostRed gs)    { ghostLocation = redsl }
                    , ghostPink   = (ghostPink gs)   { ghostLocation = pinksl }
                    , ghostCyan   = (ghostCyan gs)   { ghostLocation = cyansl }
                    , ghostOrange = (ghostOrange gs) { ghostLocation = orangesl }
                    }
                else gs
            Nothing   -> gs

-- goes through the list of ghosts and checks them one by one with pac-man and returns whether or not one is colliding with pac-man
checkEnemyCollision :: GameState -> Field -> Board -> Bool
checkEnemyCollision (GameState 
    { ghostRed    = (Ghost { ghostLocation = redl })
    , ghostPink   = (Ghost { ghostLocation = pinkl })
    , ghostCyan   = (Ghost { ghostLocation = cyanl })
    , ghostOrange = (Ghost { ghostLocation = orangel })
    }) pacf b = foldr ((||) . (\ghostl -> ghostInSameField pacf ghostl b)) False [redl,pinkl,cyanl,orangel]

-- takes pac-mans field and a ghosts location then checks whether or not they are in the same field
ghostInSameField :: Field -> GhostLocation -> Board -> Bool
ghostInSameField pacf ghostl b = let ghostf = locationToField ghostl b
                                 in case ghostf of
                                    Just ghostf -> pacf == ghostf
                                    Nothing     -> False

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

