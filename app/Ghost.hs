module Ghost where
import Types as T
import Entity
import Board

-- moveGhost :: Board -> GhostLocation -> Speed -> GhostLocation
-- moveGhost b l :: 

-- findOrientation :: Board -> CurrentField -> TargetFieldCord -> OldOrientation -> MustReverse -> NewOrientation

-- findTargetField :: PlayState -> GhostType -> TargetFieldCord

-- findRandomTargetField :: Board -> CurrentField -> OldOrientation -> TargetFieldCord

--TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = locationCordToFieldCord p

--TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location (x,y) T.Up)    = locationCordToFieldCord
findTargetFieldPink (Location (x,y) T.Right) = (x + 4, y)
findTargetFieldPink (Location (x,y) T.Down)  = (x, y + 4)
findTargetFieldPink (Location (x,y) T.Left)  = (x - 4, y)

--TargetField is the field mirrored to the red ghost from 2 ahead of PacMan
findTargetFieldBlue :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldBlue (Location p o)  (Location g _) = newCord(p o)
    where
        newCord :: FieldCord -> Orientation -> FieldCord
        newCord (x,y) T.Up    = (px,py - 2)
        newCord (x,y) T.Right = (px + 2,py)
        newCord (x,y) T.Down  = (px,py + 2)
        newCord (x,y) T.Left  = (px - 2,py)


-- findTargetOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
