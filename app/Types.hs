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

instance Eq Field where
    (==) :: Field -> Field -> Bool
    MkField pos fieldType == MkField pos' fieldType' = pos == pos' && fieldType == fieldType'

data FieldType = Wall | Pellet | PowerUp | Cherry | Empty
instance Show FieldType where 
    show :: FieldType -> String
    show Wall = "Wall"
    show Pellet = "Pellet"
    show PowerUp = "PowerUp"
    show Cherry = "Cherry"
    show Empty = "Empty"

instance Eq FieldType where
    (==) :: FieldType -> FieldType -> Bool
    Wall == Wall = True
    Pellet == Pellet = True
    PowerUp == PowerUp = True
    Cherry == Cherry = True
    Empty == Empty = True

type CurrentField = Field
type GhostLocation = Field
type PacManLocation = Field
type IsNotWall = Bool

data PlayState = PlayState { board       :: Board
                            ,score       :: Score
                            ,pacman      :: PacMan
                            ,ghostMode   :: GhostMode
                            ,ghostRed    :: Ghost
                            ,ghostPink   :: Ghost
                            ,ghostCyan   :: Ghost
                            ,ghostOrange :: Ghost
                           }

newtype Score = Score Natural

data GhostMode = Chase | Scatter | Frightened

data PacMan = PacMan { location           :: Location
                      ,previousOrentation :: Orientation
                      ,lives              :: Lives
                      ,speed              :: Speed
                     }

data Location = Location (Float,Float) Orientation

instance Show Location where
    show :: Location -> String
    show (Location (f,f') o) = "(" ++ show f ++ "," ++ show f' ++ ")" ++ show o

data Orientation = Up | Down | Left | Right

instance Show Orientation where
    show :: Orientation -> String
    show Up = "Up"
    show Down = "Down"
    show Types.Left = "Left"
    show Types.Right = "Right"

newtype Lives = Lives Natural
newtype Speed = Speed Natural

data Ghost = Ghost { glocation    :: Location 
                    ,targetField :: TargetField
                    ,gspeed       :: Speed
                    ,baseField   :: BaseField
                    ,mustReverse :: MustReverse
                   }

type MustReverse = Bool
type BaseField = Field
type TargetField = Field