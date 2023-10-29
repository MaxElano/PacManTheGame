{-# language CPP #-}

module PacMan where
import Types
import Board
import Entity

-- Moves pacman into a desired orientation, also checks for walls
movePacMan :: Board -> PacMan -> NewOrientation  -> ElapsedTime -> Location
movePacMan b p@(PacMan {pacManLocation = l@(Location cords _),
                        lives          = lvs,
                        pacManSpeed    = s,
                        pacManSize     = size}) no t = let nl = moveEntity (Location cords no) s t
                                                           check = boundaryCheck b p no
                                                       in (if check then l else nl)

-- Checks if the edge of pacman is in a wall or not in the new location
boundaryCheck :: Board -> PacMan -> NewOrientation -> IsWall
boundaryCheck b p@(PacMan {pacManLocation = (Location cords _)
                          ,pacManSize     = size}) no = let nf = locationToField (Location (getBoundaryLocation cords no size) Up) b
                                                        in maybe True wallCheck nf

-- Gives the coordinates of the edge of pacman in the new location
getBoundaryLocation :: LocationCord -> Orientation -> Size -> LocationCord
getBoundaryLocation (x,y) Up size = (x,y - fromIntegral (size `div` 2))
getBoundaryLocation (x,y) Types.Right size = (x + fromIntegral (size `div` 2),y)
getBoundaryLocation (x,y) Down size = (x,y + fromIntegral (size `div` 2))
getBoundaryLocation (x,y) Types.Left size = (x - fromIntegral (size `div` 2),y)

