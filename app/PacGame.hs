{-# language CPP #-}

module PacMan where
import Types
import Board
import Entity

movePacMan :: Board -> Location -> NewOrientation -> Speed -> Location
movePacMan b l no s = let nl = move (determineOrientation b l no) s in locationToField nl case
                                                                                            -> 

determineOrientation :: Board -> Location -> NewOrientation -> Location
determineOrientation b l@(MkLocation (x,y) oo) oo = l
determineOrientation b   (MkLocation c@(x,y) oo) no | wallCheck (b getNextFieldLocation c no) = MkLocation c no

getNextFieldLocation :: FieldCoordinate -> Orientation -> FieldCoordinate
getNextFieldLocation (x,y) Up = y - 1
getNextFieldLocation (x,y) Right = x + 1
getNextFieldLocation (x,y) Down = y + 1
getNextFieldLocation (x,y) Left = x - 1

getField :: Board -> FieldCoordinate