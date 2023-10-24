{-# language CPP #-}

module Types where

import Prelude
import GHC.Natural (Natural)

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

data PacMan = PacMan { location           :: PacManLocation
                      ,previousOrentation :: OldOrientation
			          ,orientation        :: NewOrientation
 		              ,lives              :: Lives
                      ,speed              :: Speed
			         }