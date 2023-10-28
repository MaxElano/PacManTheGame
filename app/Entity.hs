module Entity where
import Types as T

moveEntity ::  Location -> Speed -> ElapsedTime -> Location
moveEntity l                        0 _ = l
moveEntity (Location (x,y) T.Up)    s t = Location (x,y - s * t) T.Up
moveEntity (Location (x,y) T.Right) s t = Location (x,y - s * t) T.Up
moveEntity (Location (x,y) T.Down)  s t = Location (x,y - s * t) T.Up
moveEntity (Location (x,y) T.Left)  s t = Location (x,y - s * t) T.Up 

oppositeOrientation :: Orientation -> Orientation
oppositeOrientation T.Up    = T.Down
oppositeOrientation T.Right = T.Left
oppositeOrientation T.Down  = T.Up
oppositeOrientation T.Left  = T.Right