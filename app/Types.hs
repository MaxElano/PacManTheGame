{-# language CPP #-}

module Types where

import Prelude
import GHC.Natural (Natural)
import Text.XHtml (base)

type Board = [Row]
type Row   = [Field]
data Field = MkField (Int,Int) FieldType
instance Show Field where 
    show :: Field -> String
    show (MkField (x,y) s) = "(" ++ show x ++ "," ++ show y ++ ")" ++ show s

data FieldType = Wall | Pellet | PowerUp | Cherry | Empty
instance Show FieldType where 
    show Wall = "Wall"
    show Pellet = "Pellet"
    show PowerUp = "PowerUp"
    show Cherry = "Cherry"
    show Empty = "Empty"

type FieldCoordinate = (Int, Int)

data PlayState = PlayState { board       :: Board
                            ,score       :: Score
                            ,pacman      :: PacMan
                            ,ghostMode   :: GhostMode
                            ,ghostRed    :: Ghost
                            ,ghostPink   :: Ghost
                            ,ghostCyan   :: Ghost
                            ,ghostOrange :: Ghost
                           }

newtype Score = MkScore Natural

data GhostMode = Chase | Scatter | Frightened

data PacMan = PacMan { location           :: Location
                      ,previousOrentation :: Orientation
                      ,lives              :: Lives
                      ,speed              :: Speed
                     }

data Location = MkLocation (Float,Float) Orientation
data Orientation = Up | Down | Left | Right
newtype Lives = MkLives Natural
newtype Speed = MkSpeed Natural

data Ghost = Ghost { location    :: Location 
                    ,targetField :: TargetField
                    ,speed       :: Speed
                    ,baseField   :: BaseField
                    ,mustReverse :: MustReverse
}

type MustReverse = Bool
type BaseField = Field
type TargetField = Field