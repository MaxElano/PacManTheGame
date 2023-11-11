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
      Size, Ghost (ghostLocation, Ghost, ghostStartLocation), GhostLocation, Lives (Lives), PacManAnimation (..), pacManAnimationSpeed, pacManMouthSize, InfoToShow (ShowAChar), GhostColorTo (..) )
import Board 
    ( locationToField, 
      wallCheck, 
      changeFieldType )
import Entity ( moveEntity, getCornerBoundaryLocations, boundaryCheck, snapToCenter )
import Ghost (changeAllGhostColor)

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
    | not (boundaryCheck b cords fo size) = moveEntity (Location cords fo) speed t size
    | not (boundaryCheck b cords o size)  = moveEntity l speed t size
    | otherwise                        = l

--movePacMan :: GameState -> PacManLocation
-- movePacMan gs@(GameState { elapsedTime = t
--                          , board       = b
--                          , pacMan      = p@(PacMan
--                             { pacManLocation = l@(Location cords o)
--                             , pacManFutureOrientation = fo
--                             , pacManSpeed    = speed
--                             , pacManSize     = size
--                             })
--                          }) 
--     | o == fo   = let isWall = boundaryCheck b cords o size
--                   in if isWall then snapToCenter l size else moveEntity l speed t size
--     | otherwise = let nIsWall = boundaryCheck b cords fo size
--                       oIsWall = boundaryCheck b cords o size
--                   in if nIsWall then moveEntity l speed t size else moveEntity (Location cords fo) speed t size

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

-- Changes the gamestate depending on whether or not pac-man is in the same field as a ghost, 
-- if they are, the game to the start position and pac-man loses a life
enemyCollision :: GameState -> GameState
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
    }) = let pacf = locationToField pacl b
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

deathCheck :: GameState -> GameState
deathCheck gs@(GameState { pacMan = (PacMan { lives = l }) })
    | l == Lives 0    = gs { infoToShow = ShowAChar 'L' }
    | otherwise       = gs

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

