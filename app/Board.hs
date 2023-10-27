module Board where

import Prelude
import System.IO
--import Language.Haskell.TH (safe)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Types
    ( Orientation,
      Location(..),
      IsNotWall,
      FieldType(Wall),
      Field(..),
      Row,
      Board,
      fieldSize )

locationToField :: Location -> Board -> MaybeField
locationToField (Location lcords _) b = let fcords = lCordToFCord lcords in (searchRow fcords =<< b)

searchRow :: FieldCords -> Row -> MaybeField
searchRow c = find (\(MkField c1 _) -> c1 == c)

lCordToFCord :: LocationCord -> FieldCord
lCordToFCord (x,y) = let size = fromIntegral fieldSize in (x `div` size,y `div` size)

--findRow :: Location -> Board -> Row
--findRow (Location (_,y) _) board = let headRows = map (\r@((MkField (_,y1) _):_)-> (abs(fromIntegral y1 - y),r)) board 
--                                   in snd (minimumBy (comparing fst) headRows)

--findField :: Location -> Row -> Field
--findField (Location (x,_) _) row = let fields = map (\f@(MkField (x1,_) _) -> (abs(fromIntegral x1 - x),f)) row
--                                   in snd (minimumBy (comparing fst) fields)

fieldToLocation :: Field -> Orientation -> Location
fieldToLocation = Location fCordToLCord

fCordToLCord :: FieldCord -> LocationCord
fCordToLCord (x,y) = let size = fromIntegral fieldSize in (fromIntegral x * size + size / 2, fromIntegral y * size + size / 2)

wallCheck :: Field -> IsNotWall
wallCheck (MkField _ Wall) = False
wallCheck (MkField _ _)    = True

changeFieldType :: Board -> Field -> FieldType -> Board
changeFieldType board f newType = map (\row -> fieldReplace row f newType) board

fieldReplace :: Row -> Field -> FieldType -> Row
fieldReplace [] _ _ = []
fieldReplace (f:fs) f1@(MkField pos _) newType =
    if f == f1
        then MkField pos newType:fs
        else f:fieldReplace fs f1 newType