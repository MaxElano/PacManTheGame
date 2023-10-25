{-# language CPP #-}

module PacMan where
import Types
import Data.Binary.Get (label)

move ::  Location -> Speed -> Location
move l                        0 = l
move (MkLocation (x,y) Up)    s = y - s
move (MkLocation (x,y) Right) s = x + s
move (MkLocation (x,y) Down)  s = y + s
move (MkLocation (x,y) Left)  s = x - s

