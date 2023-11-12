module LevelLoader where

import Prelude
import System.IO ()
import Types ( Board, Field(..), FieldType(..), Row )

-- returns the io board when given a filepath
loadLevel :: FilePath -> IO Board
loadLevel = makeBoard . createStringList

-- reads the levelfile and returns the io list of strings 
createStringList :: FilePath -> IO [String]
createStringList fp = readFile fp >>= \string ->
                        return (lines string)

-- makes the io board 
makeBoard :: IO [String] -> IO Board
makeBoard = fmap (convertLine 0 . reverse)
    where 
        convertLine :: Int -> [String] -> [Row]
        convertLine _ [] = []
        convertLine y (r:rs) = convertField 0 y r : convertLine (y + 1) rs
        convertField :: Int -> Int -> [Char] -> [Field]
        convertField _ _ [] = []
        convertField x y (f:fs) = MkField (x,y) (chooseFieldType f) : convertField (x + 1) y fs
        chooseFieldType :: Char -> FieldType
        chooseFieldType 'W' = Wall
        chooseFieldType 'G' = GhostWall
        chooseFieldType '+' = Pellet
        chooseFieldType 'P' = PowerUp
        chooseFieldType 'C' = Cherry
        chooseFieldType '.' = Empty
        chooseFieldType _ = Empty
