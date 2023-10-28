module Ghost (moveAllGhosts, findAllTargetFields) where
import Types as T
    ( TargetFieldCord,
      BaseField,
      GhostType(Orange, Red, Pink, Cyan),
      GhostLocation,
      Ghost(..),
      PacManLocation,
      PacMan(PacMan, pacmanLocation),
      Orientation(..),
      LocationCord,
      Location(Location),
      FieldCord,
      FieldType(Wall),
      Field(MkField),
      Board,
      GhostMode(Frightened),
      GameState(GameState, ghostOrange, ghostMode, generator, board,
                elapsedTime, ghostCyan, ghostPink, ghostRed, pacman),
      ElapsedTime )
import Entity ( moveEntity, oppositeOrientation )
import Board
    ( locationToField, findFieldCordAhead, useRandom, lCordToFCord )
import System.Random ( StdGen )
import GHC.Real (mkRationalBase10)

moveAllGhosts :: GameState -> GameState
moveAllGhosts gs@(GameState {ghostMode = Frightened}) = let (ngr, ng1) = moveGhostRandom (generator gs) (board gs) (ghostCyan   gs) (elapsedTime gs) 
                                                            (ngc, ng2) = moveGhostRandom ng1            (board gs) (ghostCyan   gs) (elapsedTime gs) 
                                                            (ngp, ng3) = moveGhostRandom ng2            (board gs) (ghostPink   gs) (elapsedTime gs)
                                                            (ngo, ng4) = moveGhostRandom ng3            (board gs) (ghostOrange gs) (elapsedTime gs)
                                                        in  gs {ghostRed    = ngr
                                                               ,ghostCyan   = ngc
                                                               ,ghostPink   = ngp
                                                               ,ghostOrange = ngo
                                                               ,generator   = ng4}
moveAllGhosts gs = gs {ghostRed    = moveGhost (board gs) (ghostRed    gs) (elapsedTime gs)
                      ,ghostCyan   = moveGhost (board gs) (ghostCyan   gs) (elapsedTime gs)
                      ,ghostPink   = moveGhost (board gs) (ghostPink   gs) (elapsedTime gs)
                      ,ghostOrange = moveGhost (board gs) (ghostOrange gs) (elapsedTime gs)}

moveGhost :: Board -> Ghost -> ElapsedTime -> Ghost
moveGhost b g@(Ghost {ghostLocation = l@(Location c o)
                     ,targetField   = t
                     ,mustReverse   = True
                     ,ghostSpeed    = s}) et           = g {ghostLocation = moveEntity (Location c (oppositeOrientation o)) s et
                                                           ,mustReverse = False}
moveGhost b g@(Ghost {ghostLocation = l@(Location c _)
                     ,targetField   = t
                     ,ghostSpeed    = s}) et           = g {ghostLocation = moveEntity (Location c (findOrientation b l t)) s et}
    where
        findOrientation :: Board -> GhostLocation -> TargetFieldCord -> Orientation
        findOrientation b gl@(Location gc _) tf = let ao = tryAllOrientations b gl
                                                  in case ao of
                                                     [x] -> x
                                                     xs  -> checkEveryLocation b tf (lCordToFCord gc) xs
        --Checks which new field for the ghost is the closest to his TargetField
        checkEveryLocation :: Board -> TargetFieldCord -> FieldCord -> [Orientation] -> Orientation
        checkEveryLocation b t c [x1, x2] = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                            in if d1 <= d2
                                                    then x1
                                                    else x2
        checkEveryLocation b t c (x1:x2:xs) = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                  d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                              in if d1 <= d2
                                                    then checkEveryLocation b t c (x1:xs)
                                                    else checkEveryLocation b t c (x2:xs)
        --Calculates the distance between two FieldCords in Float
        distanceFCToFloat :: FieldCord -> FieldCord -> Float
        distanceFCToFloat (px, py) (gx, gy) = sqrt (abs ((npx - ngx) * (npx - ngx) + (npy - ngy) * (npy - ngy)))
            where
                npx = fromIntegral px :: Float
                npy = fromIntegral py :: Float
                ngx = fromIntegral gx :: Float
                ngy = fromIntegral gy :: Float


moveGhostRandom :: StdGen -> Board -> Ghost -> ElapsedTime -> (Ghost, StdGen)
moveGhostRandom gs b g@(Ghost {ghostLocation = l@(Location c o)
                              ,targetField   = t
                              ,mustReverse   = True
                              ,ghostSpeed    = s}) et           = (g {ghostLocation = moveEntity (Location c (oppositeOrientation o)) s et
                                                                     ,mustReverse = False}
                                                                  , gs)
moveGhostRandom gs b g@(Ghost {ghostLocation = l@(Location c _)
                              ,targetField   = t
                              ,ghostSpeed    = s}) et           = let (no, ng) = chooseRandomDirection gs (tryAllOrientations b l)
                                                                  in (g {ghostLocation = moveEntity (Location c no) s et}
                                                                     ,ng)
    where
        --Chooses random Orientation for the ghost, used in Frightened mode
        chooseRandomDirection :: StdGen -> [Orientation] -> (Orientation, StdGen)
        chooseRandomDirection g xs = let (rn, ng) = useRandom g (0, length xs)
                                      in (xs !! rn, ng)


--Finds all allowed new orientations for the ghost
tryAllOrientations :: Board -> GhostLocation -> [Orientation]
tryAllOrientations b gl@(Location _ o) = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left])
    where
        checkPossibility :: Board -> GhostLocation -> [Orientation] -> [Orientation]
        checkPossibility b gl@(Location l@(x,y) o) (z:zs) = let Just (MkField _ t) = locationToField gl b
                                                            in case t of
                                                            Wall -> checkPossibility b gl zs
                                                            _    -> z :checkPossibility b gl zs



findAllTargetFields :: GameState -> GameState
findAllTargetFields gs = gs {ghostRed    = findTargetField (ghostRed    gs) gs
                            ,ghostCyan   = findTargetField (ghostCyan   gs) gs
                            ,ghostPink   = findTargetField (ghostPink   gs) gs
                            ,ghostOrange = findTargetField (ghostOrange gs) gs}

--Decides which algorithm to use to chase PacMan, depends on ghostType
findTargetField :: Ghost -> GameState -> Ghost
findTargetField g@(Ghost {ghostType = Red})    (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = g {targetField = findTargetFieldRed pl}
findTargetField g@(Ghost {ghostType = Pink})   (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = g {targetField = findTargetFieldPink pl}
findTargetField g@(Ghost {ghostType = Cyan})   (GameState {pacman   = (PacMan {pacmanLocation = pl}) 
                                                          ,ghostRed = (Ghost {ghostLocation = gl})})   = g {targetField = findTargetFieldCyan pl gl}
findTargetField g@(Ghost {ghostType = Orange}) (GameState {pacman   = (PacMan {pacmanLocation = pl}) 
                                                          ,ghostOrange = (Ghost {ghostLocation = gl
                                                                                ,baseField = bf})})    = g {targetField = findTargetFieldOrange pl gl bf}

--(Red Ghost) TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = lCordToFCord p

--(Pink Ghost) TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location p o)    = findFieldCordAhead (lCordToFCord p) o 4

--(Cyan Ghost) TargetField is the field mirrored to the red ghost's location from 2 ahead of PacMan
findTargetFieldCyan :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldCyan (Location p o) (Location g _) = let (npx, npy) = findFieldCordAhead (lCordToFCord p) o 2
                                                        (ngx, ngy) = lCordToFCord g
                                                    in (npx * 2 - ngx , npy * 2 - ngy)

--(Orange Ghost) Uses it's own location and pacman's location, if within 8 range -> back to base, otherwise use red algorithm
findTargetFieldOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
findTargetFieldOrange pl@(Location pc o) (Location gc _) bf | distance pc gc > 8 = findTargetFieldRed pl
                                                            | otherwise    = bf
    where
        distance :: LocationCord -> LocationCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((px - gx) * (px - gx) + (py - gy) * (py - gy)))