{-# LANGUAGE InstanceSigs #-}
module Types where

import Prelude
import GHC.Natural (Natural)
import System.Random (StdGen, mkStdGen)
import Graphics.Gloss.Data.Color
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Data.Picture

data InfoToShow = ShowNothing
                | ShowANumber Float
                | ShowAChar   Char
                | ShowABoard  Board
                | ShowPlayState

initialState :: GameState
initialState = GameState 
               ShowPlayState
               emptyBoard 
               (Score 0) 
               (PacMan (Location (180,180) Types.Right) (Location (180,180) Types.Right) (Lives 3) 10 8 (-30, 30, 2, 4) Closing)
               (Ghost (Location (164,164) Types.Down) (Location (164,164) Types.Down) (0,0) 2 (0,0) False Red 6 red red)
               (Ghost (Location (172,172) Types.Right) (Location (172,172) Types.Right) (0,0) 2 (0,0) False Pink 6 rose rose)
               (Ghost (Location (204,204) Types.Left) (Location (204,204) Types.Left) (0,0) 2 (0,0) False Cyan 6 cyan cyan)
               (Ghost (Location (212,212) Types.Up) (Location (212,212) Types.Up) (0,0) 2 (0,0) False Orange 6 orange orange)
               Chase
               0
               0
               (mkStdGen 42)


windowSize :: (Int, Int)
windowSize = (896, 760)

emptyBoard :: Board
-- emptyBoard = [[MkField (0,0) Wall, MkField (1,0) Wall, MkField (2,0) Wall, MkField (3,0) Wall]]
emptyBoard = makeBoardTemp ["WWWWWWWWWWWWWWWWWWWWWWWWWWW"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"W.........................W"
                           ,"WWWWWWWWWWWWWWWWWWWWWWWWWWW"]

---Tijdelijk
makeBoardTemp :: [String] -> Board
makeBoardTemp = convertLineTemp 0
    where 
        convertLineTemp :: Int -> [String] -> [Row]
        convertLineTemp _ [] = []
        convertLineTemp y (r:rs) = convertFieldTemp 0 y r : convertLineTemp (y + 1) rs
        convertFieldTemp :: Int -> Int -> [Char] -> [Field]
        convertFieldTemp _ _ [] = []
        convertFieldTemp x y (f:fs) = MkField (x,y) (chooseFieldTypeTemp f) : convertFieldTemp (x + 1) y fs
        chooseFieldTypeTemp :: Char -> FieldType
        chooseFieldTypeTemp 'W' = Wall
        chooseFieldTypeTemp '+' = Pellet
        chooseFieldTypeTemp 'P' = PowerUp
        chooseFieldTypeTemp 'C' = Cherry
        chooseFieldTypeTemp '.' = Empty


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
    , totalTime   :: TotalTime
    , generator   :: StdGen
    }

newtype Score = Score Natural
instance Show Score where
    show :: Score -> String
    show (Score x) = show x

data GhostMode   = Chase | Scatter | Frightened
type ElapsedTime = Float
type TotalTime   = Float
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
    , pacManPictureValues :: PacManPictureValues
    , pacManAnimation     :: PacManAnimation
    }

type PacManLocation = Location
newtype Lives       = Lives Natural

data PacManAnimation = Opening | Closing
type PacManPictureValues = (Float, Float, Float, Float)

pacManAnimationSpeed :: Float
pacManAnimationSpeed = 150

pacManMouthSize :: Float
pacManMouthSize = 40

--------------------------Ghost--------------------------
data Ghost = Ghost 
    { ghostLocation      :: Location 
    , ghostStartLocation :: Location
    , targetField        :: TargetFieldCord
    , ghostSpeed         :: Speed
    , baseField          :: BaseField
    , mustReverse        :: MustReverse
    , ghostType          :: GhostType
    , ghostSize          :: Size
    , ghostColor         :: Color
    , ghostBaseColor     :: Color
    }

type GhostLocation = Location
data GhostType     = Red | Pink | Cyan | Orange

type MustReverse     = Bool
type BaseField       = TargetFieldCord
type TargetFieldCord = FieldCord
type CurrentField    = FieldCord

data GhostColorTo = Normal | Dark

ghostDarkColor :: Color
ghostDarkColor = azure