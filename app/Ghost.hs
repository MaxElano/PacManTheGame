module Ghost where
import Types as T
import Entity
import Board

moveGhost :: Board -> GhostLocation -> TargetFieldCord -> MustReverse -> Speed -> GhostLocation
moveGhost b l@(Location c _) t m = moveEntity (Location c (findOrientation b l t m))

findOrientation :: Board -> GhostLocation -> TargetFieldCord -> MustReverse -> Orientation
findOrientation _ (Location _ oo) _ True = oppositeOrientation oo
findOrientation b gl@(Location gc _) tf _ = let ao = tryAllOrientations b gl
                                                   in case ao of
                                                      [x] -> x
                                                      xs  -> checkEveryLocation b tf (locationCordToFieldCord gc) xs
    where
        tryAllOrientations :: Board -> GhostLocation -> [Orientation]
        tryAllOrientations b gl@(Location _ o) = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left])
        checkPossibility :: Board -> GhostLocation -> [Orientation] -> [Orientation]
        checkPossibility b gl@(Location l@(x,y) o) (z:zs) = let (MkField _ t) = findField b l       --Needs to be changed to proper board functions
                                                         in case t of
                                                            Wall -> checkPossibility b gl zs
                                                            _    -> z :checkPossibility b gl zs
        checkEveryLocation :: Board -> TargetFieldCord -> FieldCord -> [Orientation] -> Orientation
        checkEveryLocation b t c [x1, x2] = let d1 = distance t (findFieldCordAhead c x1 1)
                                                d2 = distance t (findFieldCordAhead c x2 1)
                                            in if d1 <= d2
                                                  then x1
                                                  else x2
        checkEveryLocation b t c (x1:x2:xs) = let d1 = distance t (findFieldCordAhead c x1 1)
                                                  d2 = distance t (findFieldCordAhead c x2 1)
                                              in if d1 <= d2
                                                    then checkEveryLocation b t c (x1:xs)
                                                    else checkEveryLocation b t c (x2:xs)
        distance :: FieldCord -> FieldCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((npx - ngx) * (npx - ngx) + (npy - ngy) * (npy - ngy)))
            where
                npx = fromIntegral px :: Float
                npy = fromIntegral py :: Float
                ngx = fromIntegral gx :: Float
                ngy = fromIntegral gy :: Float

findTargetField :: GhostType -> GameState -> TargetFieldCord
findTargetField Red    (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = findTargetFieldRed pl
findTargetField Pink   (GameState {pacman   = (PacMan {pacmanLocation = pl})}) = findTargetFieldPink pl
findTargetField Cyan   (GameState {pacman   = (PacMan {pacmanLocation = pl})
                                  ,ghostRed = (Ghost {ghostLocation = gl})})   = findTargetFieldCyan pl gl
findTargetField Orange (GameState {pacman   = (PacMan {pacmanLocation = pl}),
                                   ghostOrange = (Ghost {ghostLocation = gl
                                                        ,baseField = bf})})    = findTargetFieldOrange pl gl bf

-- findRandomTargetField :: Board -> CurrentField -> OldOrientation -> TargetFieldCord


--TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = locationCordToFieldCord p

--TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location p o)    = findFieldCordAhead (locationCordToFieldCord p) o 4

--TargetField is the field mirrored to the red ghost's location from 2 ahead of PacMan
findTargetFieldCyan :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldCyan (Location p o) (Location g _) = let nc = findFieldCordAhead (locationCordToFieldCord p) o 2
                                                    in 2 * nc - locationCordToFieldCord g

--Uses it's own location and pacman's location, if within 8 range -> back to base, otherwise use red algorithm
findTargetFieldOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
findTargetFieldOrange pl@(Location (px, py) o) (Location (gx, gy) _) bf | distance > 8 = findTargetFieldRed pl
                                                                        | otherwise    = bf
    where
        distance :: LocationCord -> LocationCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((px - gx) * (px - gx) + (py - gy) * (py - gy)))