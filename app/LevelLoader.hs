module LevelLoader where

import Prelude
import System.IO ()
import Types ( Board, Field(..), FieldType(..), Row )

printLevel :: FilePath -> IO()
printLevel fp = do c <- loadLevel fp
                   print c

loadLevel :: FilePath -> IO Board
loadLevel = makeBoard . createStringList

createStringList :: FilePath -> IO [String]
createStringList fp = readFile fp >>= \string ->
                        return (lines string)

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
        chooseFieldType '+' = Pellet
        chooseFieldType 'P' = PowerUp
        chooseFieldType 'C' = Cherry
        chooseFieldType '.' = Empty
