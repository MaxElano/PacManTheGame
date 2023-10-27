module Ghost where
import Types as T
import Entity
import Board

-- moveGhost :: Board -> GhostLocation -> Speed -> GhostLocation
-- moveGhost b l :: 

-- findOrientation :: Board -> CurrentField -> TargetFieldCord -> OldOrientation -> MustReverse -> NewOrientation

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
        distance (px, py) (gx, gy) = sqrt (abs((px - gx) * (px - gx) + (py - gy) * (py - gy)))