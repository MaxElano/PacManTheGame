import Data.Foldable (minimumBy, find)
import Prelude

locationToField :: Location -> Board -> Maybe Field
locationToField (Location lcords _) b = let fcords = lCordToFCord lcords 
                                        in case map (searchRow fcords) b of
                                            (field:_) -> field
                                            []        -> Nothing

type Board = [Row]
type Row   = [Field]
data Field = MkField FieldCord FieldType

type LocationCord = (Float, Float)
type FieldCord = (Int, Int)
data FieldType = Wall | Pellet | PowerUp | Cherry | Empty
instance Show FieldType where 
show :: FieldType -> String
show Wall    = "Wall"
show Pellet  = "Pellet"
show PowerUp = "PowerUp"
show Cherry  = "Cherry"
show Empty   = "Empty"

data Location = Location LocationCord Orientation
data Orientation = Up | Down | Left | Right

fieldSize :: Int
fieldSize = 8

lCordToFCord :: LocationCord -> FieldCord
lCordToFCord (x,y) = (truncate x `div` fieldSize, truncate y `div`fieldSize)

searchRow :: FieldCord -> Row -> Maybe Field
searchRow c = find (\(MkField c1 _) -> c1 == c)

instance Show Field where 
    show :: Field -> String
    show (MkField (x,y) s) = "(" ++ Prelude.show x ++ "," ++ Prelude.show y ++ ")" ++ Prelude.show s


