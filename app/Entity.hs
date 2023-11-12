module Entity where
import Prelude hiding (Right, Left)
import Types
    ( Orientation(..), Location(..), Speed, ElapsedTime, LocationCord, Size, Board, IsWall, GhostHouseStatus(..) )
import qualified Types as T
import Board (isWall, locationToField, isGhostWall)

-- moves an entity if the entity won't be in a wall in the next location
moveEntity :: Board -> Location -> Speed -> ElapsedTime -> Size -> Location
moveEntity b l@(Location cords o) speed t size
    | boundaryCheck b cords o size = snapToCenter l size
    | otherwise                    = nl
    where nl = nextLocation l speed t size

-- moves the ghost depending on whether it can leave it's cage, if the next location is in a wall, etc
moveEntityGhost :: Board -> Location -> Speed -> ElapsedTime -> Size -> GhostHouseStatus -> Location
moveEntityGhost b l@(Location cords o) speed t size MayLeave
    | ghostWallBoundaryCheck b cords o size = nl
    | boundaryCheck b cords o size = snapToCenter l size
    | otherwise                    = nl
    where nl = nextLocation l speed t size
moveEntityGhost b l@(Location cords o) speed t size _
    | boundaryCheck b cords o size = snapToCenter l size
    | otherwise                    = nl
    where nl = nextLocation l speed t size

-- returns the new location depending on speed, time, size and location
nextLocation :: Location -> Speed -> ElapsedTime -> Size -> Location
nextLocation l                      0 _ _ = l
nextLocation (Location (x,y) Up)    s t size = Location (fromIntegral (centerCoordinate (truncate x) size),y + s * t) Up
nextLocation (Location (x,y) Right) s t size = Location (x + s * t,fromIntegral (centerCoordinate (truncate y) size)) Right
nextLocation (Location (x,y) Down)  s t size = Location (fromIntegral (centerCoordinate (truncate x) size),y - s * t) Down
nextLocation (Location (x,y) Left)  s t size = Location (x - s * t,fromIntegral (centerCoordinate (truncate y) size)) Left

-- checks if the entity is in a wall or not in the new location
boundaryCheck :: Board -> LocationCord -> Orientation -> Size -> IsWall
boundaryCheck b cords o s = let (l1,l2) = getCornerBoundaryLocations cords o s
                            in  maybe False isWall (locationToField l1 b) || maybe False isWall (locationToField l2 b)

-- checks if the ghost is in a wall or not in the new location
ghostWallBoundaryCheck :: Board -> LocationCord -> Orientation -> Size -> IsWall
ghostWallBoundaryCheck b cords o s = let (l1,l2) = getCornerBoundaryLocations cords o s
                                     in  maybe False isGhostWall (locationToField l1 b) && maybe False isGhostWall (locationToField l2 b)
 
-- gives the coordinates of the edge of an entity in the new location
getFrontBoundaryLocation :: Location -> Size -> LocationCord
getFrontBoundaryLocation (Location (x,y) Up)    size = (x, y + fromIntegral (size `div` 2))
getFrontBoundaryLocation (Location (x,y) Right) size = (x    + fromIntegral (size `div` 2), y)
getFrontBoundaryLocation (Location (x,y) Down)  size = (x, y - fromIntegral (size `div` 2))
getFrontBoundaryLocation (Location (x,y) Left)  size = (x    - fromIntegral (size `div` 2), y)

-- gives the coordinates of the corners of an entity in the new location
getCornerBoundaryLocations :: LocationCord -> Orientation -> Size -> (Location,Location)
getCornerBoundaryLocations (x,y) T.Up    size = (Location (x - almostHalfSize size, y + moreThanHalfSize size) Up, Location (x + almostHalfSize size, y + moreThanHalfSize size) Up)
getCornerBoundaryLocations (x,y) T.Right size = (Location (x + moreThanHalfSize size, y + almostHalfSize size) Up, Location (x + moreThanHalfSize size, y - almostHalfSize size) Up)
getCornerBoundaryLocations (x,y) T.Down  size = (Location (x - almostHalfSize size, y - moreThanHalfSize size) Up, Location (x + almostHalfSize size, y - moreThanHalfSize size) Up)
getCornerBoundaryLocations (x,y) T.Left  size = (Location (x - moreThanHalfSize size, y + almostHalfSize size) Up, Location (x - moreThanHalfSize size, y - almostHalfSize size) Up)

-- returns more than half the size
moreThanHalfSize :: Size -> Float
moreThanHalfSize size = fromIntegral size / 1.99

-- returns a little less than half the size 
almostHalfSize :: Size -> Float
almostHalfSize size = fromIntegral size / 2.05

-- snaps a location to the center of the field it is in
snapToCenter :: Location -> Size -> Location
snapToCenter (Location (x,y) o) s = Location (center (truncate x, truncate y)) o
    where center (x',y') = (fromIntegral (centerCoordinate x' s), fromIntegral (centerCoordinate y' s))

-- centers a coordinate
centerCoordinate :: Int -> Size -> Int
centerCoordinate x size = x - x `mod` size + size `div` 2

-- returns the opposite direction
oppositeOrientation :: Orientation -> Orientation
oppositeOrientation Up    = Down
oppositeOrientation Right = Left
oppositeOrientation Down  = Up
oppositeOrientation Left  = Right