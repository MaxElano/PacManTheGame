module Entity where
import Types as T

moveEntity ::  Location -> Speed -> Location
moveEntity l                        0 = l
moveEntity (Location (x,y) T.Up)    s = Location (x,y - s) T.Up
moveEntity (Location (x,y) T.Right) s = Location (x,y - s) T.Up
moveEntity (Location (x,y) T.Down)  s = Location (x,y - s) T.Up
moveEntity (Location (x,y) T.Left)  s = Location (x,y - s) T.Up

