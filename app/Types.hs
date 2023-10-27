{-# LANGUAGE InstanceSigs #-}
module Types where

import Prelude
import GHC.Natural (Natural)
import System.Random (StdGen)
--import LevelLoader (loadLevel)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowABoard  (IO Board)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

-- data GameState = GameState {
--                    infoToShow  :: InfoToShow
--                   ,elapsedTime :: Float
--                   --,board :: IO Board
--                  }

-- initialState :: GameState
-- initialState = GameState ShowNothing 0

initialState :: GameState
initialState = GameState (ShowAChar 'c') 0






--------------------------Game--------------------------
data GameState = GameState { infoToShow  :: InfoToShow
                            ,board       :: Board
                            ,score       :: Score
                            ,pacman      :: PacMan
                            ,ghostMode   :: GhostMode
                            ,ghostRed    :: Ghost
                            ,ghostPink   :: Ghost
                            ,ghostCyan   :: Ghost
                            ,ghostOrange :: Ghost
                            ,elapsedTime :: Float
                            ,generator   :: StdGen
                           }

newtype Score = Score Natural
data GhostMode = Chase | Scatter | Frightened

--------------------------Board--------------------------
type Board = [Row]
type Row   = [Field]
data Field = MkField FieldCord FieldType
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

type IsWall = Bool

type FieldCord = (Int, Int)

fieldSize :: Int
fieldSize = 8
--------------------------Entity--------------------------
type Speed = Float

data Location = Location LocationCord Orientation
instance Show Location where
    show :: Location -> String
    show (Location (f,f') o) = "(" ++ show f ++ "," ++ show f' ++ ")" ++ show o

type LocationCord = (Float, Float)

data Orientation = Up | Down | Left | Right
instance Show Orientation where
    show :: Orientation -> String
    show Up = "Up"
    show Down = "Down"
    show Types.Left = "Left"
    show Types.Right = "Right"

--------------------------PacMan--------------------------
data PacMan = PacMan { pacmanLocation     :: Location
                      ,lives              :: Lives
                      ,pacmanSpeed        :: Speed
                     }

type PacManLocation = Location
newtype Lives = Lives Natural

--------------------------Ghost--------------------------
data Ghost = Ghost { ghostLocation :: Location 
                    ,targetField   :: TargetFieldCord
                    ,ghostSpeed    :: Speed
                    ,baseField     :: BaseField
                    ,mustReverse   :: MustReverse
                    ,ghostType     :: GhostType
                   }

type GhostLocation = Location
data GhostType = Red | Pink | Cyan | Orange

type MustReverse = Bool
type BaseField = TargetFieldCord
type TargetFieldCord = FieldCord
type CurrentField = FieldCord