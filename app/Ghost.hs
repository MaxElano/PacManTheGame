module Ghost (moveAllGhosts, findAllTargetFields, changeAllGhostColor, handleGhostTimers) where
import Types as T
    ( IsWall,
      TargetFieldCord,
      BaseField,
      GhostType(Orange, Red, Pink, Cyan),
      GhostLocation,
      Ghost(..),
      PacManLocation,
      PacMan(PacMan, pacManLocation),
      Orientation(..),
      LocationCord,
      Location(Location),
      FieldCord,
      FieldType(..),
      Field(MkField),
      Board,
      GhostMode(..),
      GhostHouseStatus(..),
      GameState(GameState, ghostOrange, generator, board,
                elapsedTime, ghostCyan, ghostPink, ghostRed, pacMan, ghostHouseDoor),
      ElapsedTime, Size, GhostColorTo (..), ghostDarkColor, Time)
import Entity ( moveEntity, oppositeOrientation, boundaryCheck, ghostWallBoundaryCheck, moveEntityGhost )
import Board
    ( locationToField, findFieldCordAhead, useRandom, lCordToFCord)
import System.Random ( StdGen )
import GHC.Real (mkRationalBase10)
import Graphics.Gloss.Data.Color as C
import GHC.RTS.Flags (DebugFlags(gc), getParFlags)
import Data.Maybe

--Main Function 1 for the entire module. Handles all movement for the ghosts
moveAllGhosts :: GameState -> GameState
moveAllGhosts gs = let (ngr, ng1) = moveGhost (board gs) (ghostRed    gs) (elapsedTime gs) (ghostMode (ghostRed gs))    (generator gs)
                       (ngc, ng2) = moveGhost (board gs) (ghostCyan   gs) (elapsedTime gs) (ghostMode (ghostCyan gs))   ng1
                       (ngp, ng3) = moveGhost (board gs) (ghostPink   gs) (elapsedTime gs) (ghostMode (ghostPink gs))   ng2
                       (ngo, ng4) = moveGhost (board gs) (ghostOrange gs) (elapsedTime gs) (ghostMode (ghostOrange gs)) ng3
                   in gs
                       { ghostRed    = ngr
                       , ghostCyan   = ngc
                       , ghostPink   = ngp
                       , ghostOrange = ngo
                       , generator   = ng4
                       }

--Handles all the movement for one ghost
moveGhost :: Board -> Ghost -> ElapsedTime -> GhostMode -> StdGen -> (Ghost, StdGen)
--Reverses the ghost if neccessary
moveGhost b g@(Ghost
    { ghostLocation    = l@(Location c o)
    , mustReverse      = True
    }) et _ gen     = (g 
        { ghostLocation = moveEntityGhost b (Location c (oppositeOrientation o)) (ghostSpeed g) et (ghostSize g) (ghostHouseStatus g)
        , mustReverse   = False 
        }, gen)
--Handles random direction, when inside of GhostHouse
moveGhost b g@(Ghost
    { ghostLocation    = l@(Location c o)
    , ghostHouseStatus = Inside
    }) et _ gen = let os = (tryAllOrientations b l (ghostSize g) Inside)
                      (no, ng) = chooseRandomDirection gen os
                  in (g { ghostLocation = moveEntityGhost b (Location c no) (ghostSpeed g) et (ghostSize g) (ghostHouseStatus g) }, ng)
--Handles random direcion, when frightened
moveGhost b g@(Ghost
    { ghostLocation    = l@(Location c _)
    }) et Frightened gen = let (no, ng) = chooseRandomDirection gen (tryAllOrientations b l (ghostSize g) Outside)
                           in (g { ghostLocation = moveEntityGhost b (Location c no) (ghostSpeed g) et (ghostSize g) (ghostHouseStatus g) }, ng)
--"Normal" move
moveGhost b g@(Ghost
    { ghostLocation    = l@(Location c _)
    }) et _ gen     = let nl = moveEntityGhost b (Location c $ findOrientation b l (targetField g) (ghostSize g) (ghostHouseStatus g)) (ghostSpeed g) et (ghostSize g) (ghostHouseStatus g)
                          nh = case locationToField nl b of
                               Just (MkField _ GhostWall) -> Outside
                               _                          -> (ghostHouseStatus g)
                      in (g { ghostLocation = nl, ghostHouseStatus = nh}, gen)
        where
            --Main function for finding the new orientation for the ghost
            findOrientation :: Board -> GhostLocation -> TargetFieldCord -> Size -> GhostHouseStatus -> Orientation
            findOrientation b gl@(Location gc oo) tf s h = let ao = tryAllOrientations b gl s h
                                                           in case ao of
                                                              [x] -> x
                                                              xs  -> fromMaybe oo $ checkEveryLocation b tf (lCordToFCord gc) xs
            --Checks which new field for the ghost is the closest to his TargetField
            checkEveryLocation :: Board -> TargetFieldCord -> FieldCord -> [Orientation] -> Maybe Orientation
            checkEveryLocation b t c [x1, x2] = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                    d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                                in if d1 <= d2
                                                    then Just x1
                                                    else Just x2
            checkEveryLocation b t c (x1:x2:xs) = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                      d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                                  in if d1 <= d2
                                                    then checkEveryLocation b t c (x1:xs)
                                                    else checkEveryLocation b t c (x2:xs)
            checkEveryLocation b t c [x]       = Just x
            checkEveryLocation b t c []        = Nothing
            --Calculates the distance between two FieldCords in Float
            distanceFCToFloat :: FieldCord -> FieldCord -> Float
            distanceFCToFloat (px, py) (gx, gy) = sqrt (abs ((npx - ngx) * (npx - ngx) + (npy - ngy) * (npy - ngy)))
                where
                    npx = fromIntegral px :: Float
                    npy = fromIntegral py :: Float
                    ngx = fromIntegral gx :: Float
                    ngy = fromIntegral gy :: Float

--Chooses random direction from list
chooseRandomDirection :: StdGen -> [Orientation] -> (Orientation, StdGen)
chooseRandomDirection g [] = (Up, g)
chooseRandomDirection g xs = let (rn, ng) = useRandom g (0, length xs - 1)
                             in (xs !! rn, ng)

--Finds all allowed new orientations for the ghost
tryAllOrientations :: Board -> GhostLocation -> Size -> GhostHouseStatus -> [Orientation]
tryAllOrientations b gl@(Location _ o) s Inside   = let os = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left]) s Inside []
                                                    in case os of
                                                       [] -> [oppositeOrientation o]
                                                       _  -> os
tryAllOrientations b gl@(Location _ o) s Outside  = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left]) s Outside []
tryAllOrientations b gl@(Location _ o) s MayLeave = checkPossibility b gl [T.Up, T.Right, T.Down, T.Left] s MayLeave []

checkPossibility :: Board -> GhostLocation -> [Orientation] -> Size -> GhostHouseStatus -> [Orientation] -> [Orientation]
checkPossibility _ _                       [] _     _        acc = acc
checkPossibility b gl@(Location l@(x,y) o) (z:zs) s MayLeave acc | ghostWallBoundaryCheck b l z s = [z]
                                                                 | boundaryCheck b l z s          = checkPossibility b gl zs s MayLeave acc
                                                                 | otherwise                      = checkPossibility b gl zs s MayLeave (z : acc)
checkPossibility b gl@(Location l@(x,y) o) (z:zs) s h        acc | boundaryCheck b l z s = checkPossibility b gl zs s h acc
                                                                 | otherwise             = checkPossibility b gl zs s h (z : acc)
    

--Main Function 2 for the entire module. Finds each target field and returns them inside the new GameState
findAllTargetFields :: GameState -> GameState
findAllTargetFields gs = gs 
    { ghostRed    = assignTargetField gs (ghostRed gs) 
    , ghostPink   = assignTargetField gs (ghostPink gs)
    , ghostCyan   = assignTargetField gs (ghostCyan gs)
    , ghostOrange = assignTargetField gs (ghostOrange gs)
    }

assignTargetField :: GameState -> Ghost -> Ghost
assignTargetField _  g@(Ghost { ghostHouseStatus = Inside })   = g
assignTargetField gs g@(Ghost { ghostHouseStatus = MayLeave }) = g { targetField = ghostHouseDoor gs } 
assignTargetField gs g@(Ghost { ghostMode = Chase
                              , ghostHouseStatus = Outside })  = findTargetField g gs
assignTargetField gs g@(Ghost { ghostMode = Scatter
                              , ghostHouseStatus = Outside })  = g { targetField = baseField g }
assignTargetField _ g                                          = g 
        
--Decides which algorithm to use to chase PacMan, depends on ghostType
findTargetField :: Ghost -> GameState -> Ghost
findTargetField g@(Ghost { ghostType = Red })    (GameState { pacMan      = (PacMan { pacManLocation = pl }) }) = g { targetField = findTargetFieldRed pl }
findTargetField g@(Ghost { ghostType = Pink })   (GameState { pacMan      = (PacMan { pacManLocation = pl }) }) = g { targetField = findTargetFieldPink pl }
findTargetField g@(Ghost { ghostType = Cyan })   (GameState
    { pacMan      = (PacMan { pacManLocation = pl })
    , ghostRed    = (Ghost  { ghostLocation = gl })
    }) = g { targetField = findTargetFieldCyan pl gl }
findTargetField g@(Ghost { ghostType = Orange }) (GameState
    { pacMan      = (PacMan {pacManLocation = pl})
    , ghostOrange = (Ghost
        { ghostLocation = gl
        , baseField = bf
        })
    }) = g {targetField = findTargetFieldOrange pl gl bf}

--(Red Ghost) TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = lCordToFCord p

--(Pink Ghost) TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location p o) = findFieldCordAhead (lCordToFCord p) o 4

--(Cyan Ghost) TargetField is the field mirrored to the red ghost's location from 2 ahead of PacMan
findTargetFieldCyan :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldCyan (Location p o) (Location g _) = let (npx, npy) = findFieldCordAhead (lCordToFCord p) o 2
                                                        (ngx, ngy) = lCordToFCord g
                                                    in (npx * 2 - ngx , npy * 2 - ngy)

--(Orange Ghost) Uses it's own location and pacman's location, if within 8 range -> back to base, otherwise use red algorithm
findTargetFieldOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
findTargetFieldOrange pl@(Location pc o) (Location gc _) bf | distance pc gc > 8 = findTargetFieldRed pl
                                                            | otherwise          = bf
    where
        distance :: LocationCord -> LocationCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((px - gx) * (px - gx) + (py - gy) * (py - gy)))

changeAllGhostColor :: GameState-> GhostColorTo -> GameState
changeAllGhostColor gs c = gs { ghostRed    = changeGhostColor (ghostRed gs)  c
                              , ghostPink   = changeGhostColor (ghostPink gs) c
                              , ghostCyan   = changeGhostColor (ghostCyan gs) c
                              , ghostOrange = changeGhostColor (ghostOrange gs) c }

changeGhostColor :: Ghost -> GhostColorTo -> Ghost
changeGhostColor g T.Normal = g {ghostColor = ghostBaseColor g}
changeGhostColor g T.Dark   = g {ghostColor = ghostDarkColor}

handleGhostTimers :: Ghost -> ElapsedTime -> Ghost
handleGhostTimers g t = changeMode g 
    { ghostHouseStatus = changeGhostHouseStatus (leaveHouseTime g - t) (ghostHouseStatus g) t
    , leaveHouseTime   = leaveHouseTime g - t
    , modeTime         = modeTime g - t
    , frightenedTime   = frightenedTime g - t
    }

changeGhostHouseStatus :: Time -> GhostHouseStatus -> ElapsedTime -> GhostHouseStatus
changeGhostHouseStatus lt Inside t | lt <= 0   = MayLeave
                                   | otherwise = Inside
changeGhostHouseStatus _ g _       = g

changeMode :: Ghost -> Ghost
changeMode g@(Ghost 
    { ghostMode      = gm
    , ghostBaseColor = gbc
    , modeTime       = mt
    , coreMode       = cm
    , frightenedTime = ft
    })
    | gm == Frightened && ft <= 0 = g { ghostMode = cm, ghostColor = gbc }
    | gm == Chase && mt <= 0      = g { ghostMode = Scatter, coreMode = Scatter, modeTime = 7 }
    | gm == Scatter && mt <= 0    = g { ghostMode = Chase, coreMode = Chase, modeTime = 20 }
    | otherwise = g