module Entity where
import Types as T
--import Data.Binary.Get (label)

move ::  Location -> Speed -> Location
move l                        0 = l
move (Location (x,y) T.Up)    s = Location (x,y - s) T.Up
move (Location (x,y) T.Right) s = Location (x,y - s) T.Up
move (Location (x,y) T.Down)  s = Location (x,y - s) T.Up
move (Location (x,y) T.Left)  s = Location (x,y - s) T.Up

