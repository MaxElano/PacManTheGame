-- | This module defines how to turn
--   the game state into a picture
module Draw where

import Graphics.Gloss
import Types
import Board (fCordToLCord)

draw :: GameState -> IO Picture
draw gs = return $ drawPure gs


drawPure :: GameState -> Picture
drawPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowABoard  b -> drawBoard b
    --pictures [color red (text "A"), translate 20 20 (color green (circle 10))]
  --ShowAChar   c -> color green (text [c])
    -- pictures $ fmap (pictures . map (\(MkField (x,y) ft) -> case ft of
--                                                                 Wall -> color green (text (show 0))
--                                     )) b
--    color green (text (show "9"))

drawBoard :: Board -> Picture
drawBoard b = pictures $ map (\(MkField c t) -> let (lx, ly) = fCordToLCord c 
                                                 in case t of
                                                    Wall -> translate lx ly (color red (circle (fromIntegral fieldSize / 2)))
                             ) (concat b)