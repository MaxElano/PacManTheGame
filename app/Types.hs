{-# LANGUAGE InstanceSigs #-}
module Types where

import Prelude
import GHC.Natural (Natural)
import System.Random (StdGen, mkStdGen)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowABoard  Board

nO_SECS_BETWEEN_CYCLES :: ElapsedTime
nO_SECS_BETWEEN_CYCLES = 0

initialState :: GameState
initialState = GameState 
               ShowNothing
               emptyBoard 
               (Score 0) 
               (PacMan (Location (20,20) Up) (Location (20,20) Up) (Lives 3) 10 8)
               (Ghost (Location (20,20) Types.Down) (Location (20,20) Types.Down) (0,0) 2 (0,0) False Red)
               (Ghost (Location (20,20) Types.Right) (Location (20,20) Types.Right) (0,0) 2 (0,0) False Pink)
               (Ghost (Location (20,20) Types.Left) (Location (20,20) Types.Left) (0,0) 2 (0,0) False Cyan)
               (Ghost (Location (20,20) Types.Up) (Location (20,20) Types.Up) (0,0) 2 (0,0) False Orange)
               Chase
               0
               (mkStdGen 42)

emptyBoard :: Board
emptyBoard = [[MkField (0,0) Wall, MkField (1,0) Wall, MkField (2,0) Wall]]

--------------------------Game--------------------------
data GameState = GameState 
    { infoToShow  :: InfoToShow
    , board       :: Board   --Moet waarschijnlijk nog IO Board worden
    , score       :: Score
    , pacMan      :: PacMan
    , ghostRed    :: Ghost
    , ghostPink   :: Ghost
    , ghostCyan   :: Ghost
    , ghostOrange :: Ghost
    , ghostMode   :: GhostMode
    , elapsedTime :: ElapsedTime
    , generator   :: StdGen
    }

newtype Score    = Score Natural
data GhostMode   = Chase | Scatter | Frightened
type ElapsedTime = Float

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
    show Wall    = "Wall"
    show Pellet  = "Pellet"
    show PowerUp = "PowerUp"
    show Cherry  = "Cherry"
    show Empty   = "Empty"

instance Eq FieldType where
    (==) :: FieldType -> FieldType -> Bool
    Wall    == Wall    = True
    Pellet  == Pellet  = True
    PowerUp == PowerUp = True
    Cherry  == Cherry  = True
    Empty   == Empty   = True

type IsWall = Bool

type FieldCord = (Int, Int)

fieldSize :: Int
fieldSize = 8
--------------------------Entity--------------------------
type Speed = Float
type Size  = Int

data Location = Location LocationCord Orientation

instance Show Location where
    show :: Location -> String
    show (Location (f,f') o) = "(" ++ show f ++ "," ++ show f' ++ ")" ++ show o

instance Eq Location where
    (==) :: Location -> Location -> Bool
    Location cords orientation == Location cords' orientation' = cords == cords' && orientation == orientation'

type LocationCord = (Float, Float)

data Orientation = Up | Down | Left | Right
instance Show Orientation where
    show :: Orientation -> String
    show Types.Up          = "Up"
    show Types.Down        = "Down"
    show Types.Left        = "Left"
    show Types.Right       = "Right"

instance Eq Orientation where
    (==) :: Orientation -> Orientation -> Bool
    Types.Up    == Types.Up    = True
    Types.Right == Types.Right = True
    Types.Down  == Types.Down  = True
    Types.Left  == Types.Left  = True

type NewOrientation = Orientation

--------------------------PacMan--------------------------
data PacMan = PacMan 
    { pacManLocation      :: Location
    , pacManStartLocation :: Location
    , lives               :: Lives
    , pacManSpeed         :: Speed
    , pacManSize          :: Size
    }

type PacManLocation = Location
newtype Lives       = Lives Natural

--------------------------Ghost--------------------------
data Ghost = Ghost 
    { ghostLocation      :: Location 
    , ghostStartLocation :: Location
    , targetField        :: TargetFieldCord
    , ghostSpeed         :: Speed
    , baseField          :: BaseField
    , mustReverse        :: MustReverse
    , ghostType          :: GhostType
    }

type GhostLocation = Location
data GhostType     = Red | Pink | Cyan | Orange

type MustReverse     = Bool
type BaseField       = TargetFieldCord
type TargetFieldCord = FieldCord
type CurrentField    = FieldCord