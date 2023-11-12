module Board where

import Prelude
import System.IO ()
import Data.Foldable (minimumBy, find)
import Data.Ord (comparing)
import System.Random ( uniformR, StdGen )
import Types as T
    ( InfoToShow(..),
      Orientation,
      Location(..),
      IsWall,
      FieldType(..),
      Field(..),
      Row,
      Board,
      fieldSize,
      FieldCord,
      LocationCord,
      Orientation(..),
      Finished,
      GameState(..), Lives (Lives), PacMan (lives))
import Data.List ()
import GHC.IO.Exception (ExitCode(ExitSuccess))

-- attempts to convert a location to a field when given a board aswell
locationToField :: Location -> Board -> Maybe Field
locationToField (Location lcords _) b = let (x,y) = lCordToFCord lcords in checkY x y b
    where
        checkY :: Int -> Int -> Board -> Maybe Field
        checkY x y b | y < length b  = checkX x (b !! y) 
                     | otherwise     = Nothing
        checkX :: Int -> Row -> Maybe Field
        checkX x r | x < length r = Just (r !! x)
                   | otherwise      = Nothing

-- converts a location coordinate to a field coordinate
lCordToFCord :: LocationCord -> FieldCord
lCordToFCord (x,y) = (truncate x `div` fieldSize, truncate y `div` fieldSize)

-- converts a field coordinate to a location coordinate
fCordToLCord :: FieldCord -> LocationCord
fCordToLCord (x,y) = let size = fromIntegral fieldSize 
                     in (fromIntegral x * size + size / 2, fromIntegral y * size + size / 2)

-- returns whether or not the given field is a wall
isWall :: Field -> IsWall
isWall (MkField _ Wall)      = True
isWall (MkField _ GhostWall) = True
isWall (MkField _ _)         = False

-- returns whether or not the given field is a ghost wall
isGhostWall :: Field -> IsWall
isGhostWall (MkField _ GhostWall) = True
isGhostWall (MkField _ _)         = False

-- changes the given field in the board to the desired fieldtype and then returns the new board
changeFieldType :: Board -> Field -> FieldType -> Board
changeFieldType board f newType = map (fieldReplace f newType) board

-- helper function that checks if the desired field is in this row and then changes its fieldtype
fieldReplace :: Field -> FieldType -> Row -> Row
fieldReplace _ _ []                            = []
fieldReplace f1@(MkField pos _) newType (f:fs)
    | f == f1   = MkField pos newType:fs
    | otherwise = f:fieldReplace f1 newType fs

-- finds the next field coordinate when given a field coordinate
findFieldCordAhead :: FieldCord -> Orientation -> Int -> FieldCord
findFieldCordAhead (x,y) T.Up    i = (x, y + i)
findFieldCordAhead (x,y) T.Right i = (x + i, y)
findFieldCordAhead (x,y) T.Down  i = (x, y - i)
findFieldCordAhead (x,y) T.Left  i = (x - i, y)

-- uses the generator and returns a new one
useRandom :: StdGen -> (Int, Int) -> (Int, StdGen)
useRandom g r = uniformR r g