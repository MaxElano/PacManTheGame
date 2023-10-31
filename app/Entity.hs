module Entity where
import Prelude hiding (Right, Left)
import Types
    ( Orientation(..), Location(..), Speed, ElapsedTime )

moveEntity ::  Location -> Speed -> ElapsedTime -> Location
moveEntity l                      0 _ = l
moveEntity (Location (x,y) Up)    s t = Location (x,y + s * t) Up
moveEntity (Location (x,y) Right) s t = Location (x + s * t,y) Right
moveEntity (Location (x,y) Down)  s t = Location (x,y - s * t) Down
moveEntity (Location (x,y) Left)  s t = Location (x - s * t,y) Left 

oppositeOrientation :: Orientation -> Orientation
oppositeOrientation Up    = Down
oppositeOrientation Right = Left
oppositeOrientation Down  = Up
oppositeOrientation Left  = Right