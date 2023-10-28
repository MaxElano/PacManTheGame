module Board where

import Prelude
import System.IO
--import Language.Haskell.TH (safe)
import Data.Foldable (minimumBy, find)
import Data.Ord (comparing)
import System.Random
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
locationToField (Location lcords _) b = let fcords = lCordToFCord lcords 
                                        in case map (searchRow fcords) b of
                                            (field:_) -> field
                                            []        -> Nothing

searchRow :: FieldCord -> Row -> Maybe Field
searchRow c = find (\(MkField c1 _) -> c1 == c)

lCordToFCord :: LocationCord -> FieldCord
lCordToFCord (x,y) = (truncate x `div` fieldSize,truncate y `div` fieldSize)

fieldToLocation :: Field -> Orientation -> Location
fieldToLocation (MkField fcords _) = Location (fCordToLCord fcords)

fCordToLCord :: FieldCord -> LocationCord
fCordToLCord (x,y) = let size = fromIntegral fieldSize in (fromIntegral x * size + size / 2, fromIntegral y * size + size / 2)

wallCheck :: Field -> IsWall
wallCheck (MkField _ Wall) = True
wallCheck (MkField _ _)    = False

changeFieldType :: Board -> Field -> FieldType -> Board
changeFieldType board f newType = map (\row -> fieldReplace row f newType) board

fieldReplace :: Row -> Field -> FieldType -> Row
fieldReplace [] _ _ = []
fieldReplace (f:fs) f1@(MkField pos _) newType = 
    if f == f1
        then MkField pos newType:fs
        else f:fieldReplace fs f1 newType

findFieldCordAhead :: FieldCord -> Orientation -> Int -> FieldCord
findFieldCordAhead (x,y) T.Up    i = (x,y - i)
findFieldCordAhead (x,y) T.Right i = (x + i,y)
findFieldCordAhead (x,y) T.Down  i = (x,y + i)
findFieldCordAhead (x,y) T.Left  i = (x - i,y)

useRandom :: StdGen -> (Int, Int) -> (Int, StdGen)
useRandom g r = let (rn, ng) = uniformR r g in (rn, g)