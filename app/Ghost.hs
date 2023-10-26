module Ghost where
import Types
import Entity
moveGhost :: Board -> GhostLocation -> Speed -> GhostLocation
moveGhost b l :: 

findOrientation :: Board -> CurrentField -> TargetField -> OldOrientation -> MustReverse -> NewOrientation

findTargetField :: PlayState -> GhostType -> TargetField

findRandomTargetField :: Board -> CurrentField -> OldOrientation -> TargetField

findTargetFieldRed :: PacManLocation -> TargetField
findTargetFieldRed = locationToField

findTargetFieldPink :: PacManLocation -> Orientation -> TargetField

findTargetFieldBlue :: PacManLocation -> Orientation -> GhostLocation -> TargetField

findTargetOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetField