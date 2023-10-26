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
      Board )

locationToField :: Location -> Board -> Field
locationToField l = findField l . findRow l

findRow :: Location -> Board -> Row
findRow (Location (_,y) _) board = let headRows = map (\r@((MkField (_,y1) _):_)-> (abs(fromIntegral y1 - y),r)) board 
                                   in snd (minimumBy (comparing fst) headRows)

findField :: Location -> Row -> Field
findField (Location (x,_) _) row = let fields = map (\f@(MkField (x1,_) _) -> (abs(fromIntegral x1 - x),f)) row
                                   in snd (minimumBy (comparing fst) fields)

fieldToLocation :: Field -> Orientation -> Location
fieldToLocation (MkField (x,y) _) = Location (fromIntegral x,fromIntegral y)

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