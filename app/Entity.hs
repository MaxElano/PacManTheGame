module Entity where
import Prelude hiding (Right, Left)
import Types
    ( Orientation(..), Location(..), Speed, ElapsedTime, LocationCord, Size, Board, IsWall )
import qualified Types as T
import Board (wallCheck, locationToField)

moveEntity ::  Location -> Speed -> ElapsedTime -> Size -> Location
moveEntity l                      0 _ _ = l
moveEntity (Location (x,y) Up)    s t size = Location (fromIntegral (centerCoordinate (truncate x) size),y + s * t) Up
moveEntity (Location (x,y) Right) s t size = Location (x + s * t,fromIntegral (centerCoordinate (truncate y) size)) Right
moveEntity (Location (x,y) Down)  s t size = Location (fromIntegral (centerCoordinate (truncate x) size),y - s * t) Down
moveEntity (Location (x,y) Left)  s t size = Location (x - s * t,fromIntegral (centerCoordinate (truncate y) size)) Left

-- Checks if the entity is in a wall or not in the new location
boundaryCheck :: Board -> LocationCord -> Orientation -> Size -> IsWall
boundaryCheck b cords o s = let (l1,l2) = getCornerBoundaryLocations cords o s
                            in  maybe False wallCheck (locationToField l1 b) || maybe False wallCheck (locationToField l2 b)

-- Gives the coordinates of the edge of pacman in the new location
getFrontBoundaryLocation :: Location -> Size -> LocationCord
getFrontBoundaryLocation (Location (x,y) Up)    size = (x, y + fromIntegral (size `div` 2))
getFrontBoundaryLocation (Location (x,y) Right) size = (x    + fromIntegral (size `div` 2), y)
getFrontBoundaryLocation (Location (x,y) Down)  size = (x, y - fromIntegral (size `div` 2))
getFrontBoundaryLocation (Location (x,y) Left)  size = (x    - fromIntegral (size `div` 2), y)

getCornerBoundaryLocations :: LocationCord -> Orientation -> Size -> (Location,Location)
getCornerBoundaryLocations (x,y) T.Up    size = (Location (x - almostHalfSize size, y + halfSize size) Up, Location (x + almostHalfSize size, y + halfSize size) Up)
getCornerBoundaryLocations (x,y) T.Right size = (Location (x + halfSize size, y + almostHalfSize size) Up, Location (x + halfSize size, y - almostHalfSize size) Up)
getCornerBoundaryLocations (x,y) T.Down  size = (Location (x - almostHalfSize size, y - halfSize size) Up, Location (x + almostHalfSize size, y - halfSize size) Up)
getCornerBoundaryLocations (x,y) T.Left  size = (Location (x - halfSize size, y + almostHalfSize size) Up, Location (x - halfSize size, y - almostHalfSize size) Up)

halfSize :: Size -> Float
halfSize size = fromIntegral size / 1.99

almostHalfSize :: Size -> Float
almostHalfSize size = fromIntegral size / 2.01

snapToCenter :: Location -> Size -> Location
snapToCenter (Location (x,y) o) s = Location (center (truncate x, truncate y)) o
    where center (x',y') = (fromIntegral (centerCoordinate x' s), fromIntegral (centerCoordinate y' s))

centerCoordinate :: Int -> Size -> Int
centerCoordinate x size = x - x `mod` size + size `div` 2

oppositeOrientation :: Orientation -> Orientation
oppositeOrientation Up    = Down
oppositeOrientation Right = Left
oppositeOrientation Down  = Up
oppositeOrientation Left  = Right