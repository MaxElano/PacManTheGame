module Ghost where
import Types as T
import Entity
import Board
import System.Random
import GHC.Real (mkRationalBase10)


-- moveAllGhosts :: GameState -> GameState
-- moveAllGhosts gs@(GameState {board       = b 
--                             ,ghostRed    = gr
--                             ,ghostCyan   = gc
--                             ,ghostPink   = gp
--                             ,ghostOrange = go
--                             ,ghostMode   = gm}) = gs {ghostRed    = moveGhost b gr gm
--                                                      ,ghostCyan   = moveGhost b gc gm
--                                                      ,ghostPink   = moveGhost b gp gm
--                                                      ,ghostOrange = moveGhost b go gm}

moveAllGhosts :: GameState -> GameState
moveAllGhosts gs = gs {ghostRed    = moveGhost (board gs) (ghostRed    gs) (ghostMode gs)
                      ,ghostCyan   = moveGhost (board gs) (ghostCyan   gs) (ghostMode gs)
                      ,ghostPink   = moveGhost (board gs) (ghostPink   gs) (ghostMode gs)
                      ,ghostOrange = moveGhost (board gs) (ghostOrange gs) (ghostMode gs)}

moveGhost :: Board -> Ghost -> GhostMode -> Ghost
moveGhost b g@(Ghost {ghostLocation = l@(Location c o)
                     ,targetField   = t
                     ,mustReverse   = True
                     ,ghostSpeed    = s}) m            = g {ghostLocation = moveEntity (Location c (oppositeOrientation o)) s
                                                           ,mustReverse = False}
moveGhost b g@(Ghost {ghostLocation = l@(Location c _)
                     ,targetField   = t
                     ,ghostSpeed    = s}) m            = g {ghostLocation = moveEntity (Location c (findOrientation b l t m)) s}
    where
        findOrientation :: Board -> GhostLocation -> TargetFieldCord -> GhostMode -> Orientation
        findOrientation b gl@(Location gc _) tf Frightened = let ao = tryAllOrientations b gl
                                                             in chooseRandomDirection
        findOrientation b gl@(Location gc _) tf _          = let ao = tryAllOrientations b gl
                                                             in case ao of
                                                                  [x] -> x
                                                                  xs  -> checkEveryLocation b tf (lCordToFCord gc) xs


-- Change to entire gamestate
-- moveGhost :: Board -> GhostLocation -> TargetFieldCord -> MustReverse -> GhostMode -> Speed -> GhostLocation
-- moveGhost b l@(Location c _) t m = moveEntity (Location c (findOrientation b l t m gm))
    -- where
        -- findOrientation :: Board -> GhostLocation -> TargetFieldCord -> MustReverse -> GhostMode -> Orientation
        -- findOrientation _ (Location _ oo) _ True _           = oppositeOrientation oo
        -- findOrientation b gl@(Location gc _) tf _ Frightened = let ao = tryAllOrientations b gl
                                                            --    in chooseRandomDirection
        -- findOrientation b gl@(Location gc _) tf _ _          = let ao = tryAllOrientations b gl
                                                            --    in case ao of
                                                                --   [x] -> x
                                                                --   xs  -> checkEveryLocation b tf (lCordToFCord gc) xs

--Finds all allowed new orientations for the ghost
tryAllOrientations :: Board -> GhostLocation -> [Orientation]
tryAllOrientations b gl@(Location _ o) = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left])
    where
        checkPossibility :: Board -> GhostLocation -> [Orientation] -> [Orientation]
        checkPossibility b gl@(Location l@(x,y) o) (z:zs) = let Just (MkField _ t) = locationToField gl b
                                                            in case t of
                                                            Wall -> checkPossibility b gl zs
                                                            _    -> z :checkPossibility b gl zs

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

--Chooses random Orientation for the ghost, used in Frightened mode
chooseRandomDirection :: [Orientation] -> Int -> Orientation
chooseRandomDirection = (!!)

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

-- --Decides which algorithm to use to chase PacMan, depends on ghostType
-- findTargetField :: GhostType -> GameState -> TargetFieldCord
-- findTargetField Red    (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = findTargetFieldRed pl
-- findTargetField Pink   (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = findTargetFieldPink pl
-- findTargetField Cyan   (GameState {pacman   = (PacMan {pacmanLocation = pl})
--                                   ,ghostRed = (Ghost {ghostLocation = gl})})   = findTargetFieldCyan pl gl
-- findTargetField Orange (GameState {pacman   = (PacMan {pacmanLocation = pl}),
--                                    ghostOrange = (Ghost {ghostLocation = gl
--                                                         ,baseField = bf})})    = findTargetFieldOrange pl gl bf

--(Red Ghost) TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = lCordToFCord p

--(Pink Ghost) TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location p o)    = findFieldCordAhead (lCordToFCord p) o 4

--(Cyan Ghost) TargetField is the field mirrored to the red ghost's location from 2 ahead of PacMan
findTargetFieldCyan :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldCyan (Location p o) (Location g _) = let nc = findFieldCordAhead (lCordToFCord p) o 2
                                                    in 2 * nc - lCordToFCord g

--(Orange Ghost) Uses it's own location and pacman's location, if within 8 range -> back to base, otherwise use red algorithm
findTargetFieldOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
findTargetFieldOrange pl@(Location (px, py) o) (Location (gx, gy) _) bf | distance > 8 = findTargetFieldRed pl
                                                                        | otherwise    = bf
    where
        distance :: LocationCord -> LocationCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((px - gx) * (px - gx) + (py - gy) * (py - gy)))