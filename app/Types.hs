{-# language CPP #-}

module Types where

import Prelude

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