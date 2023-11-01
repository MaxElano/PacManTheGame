module Board where

import Prelude
import System.IO ()
import Data.Foldable (minimumBy, find)
import Data.Ord (comparing)
import System.Random ( uniformR, StdGen )
import Types as T
    ( Orientation,
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
      GameState(..))
import Data.List

locationToField :: Location -> Board -> Maybe Field
locationToField (Location lcords _) b = let (x,y) = lCordToFCord lcords in checkY x y b
    where
        checkY :: Int -> Int -> Board -> Maybe Field
        checkY x y b | y < length b  = checkX x (b !! y) 
                     | otherwise     = Nothing
        checkX :: Int -> Row -> Maybe Field
        checkX x r | x < length r = Just (r !! x)
                   | otherwise      = Nothing

lCordToFCord :: LocationCord -> FieldCord
lCordToFCord (x,y) = (truncate x `div` fieldSize, truncate y `div` fieldSize)

fieldToLocation :: Field -> Orientation -> Location
fieldToLocation (MkField fcords _) = Location (fCordToLCord fcords)

fCordToLCord :: FieldCord -> LocationCord
fCordToLCord (x,y) = let size = fromIntegral fieldSize 
                     in (fromIntegral x * size + size / 2, fromIntegral y * size + size / 2)

wallCheck :: Field -> IsWall
wallCheck (MkField _ Wall) = True
wallCheck (MkField _ _)    = False

changeFieldType :: Board -> Field -> FieldType -> Board
changeFieldType board f newType = map (fieldReplace f newType) board

fieldReplace :: Field -> FieldType -> Row -> Row
fieldReplace _ _ []                            = []
fieldReplace f1@(MkField pos _) newType (f:fs)
    | f == f1   = MkField pos newType:fs
    | otherwise = f:fieldReplace f1 newType fs

findFieldCordAhead :: FieldCord -> Orientation -> Int -> FieldCord
findFieldCordAhead (x,y) T.Up    i = (x, y + i)
findFieldCordAhead (x,y) T.Right i = (x + i, y)
findFieldCordAhead (x,y) T.Down  i = (x, y - i)
findFieldCordAhead (x,y) T.Left  i = (x - i, y)

useRandom :: StdGen -> (Int, Int) -> (Int, StdGen)
useRandom g r = uniformR r g