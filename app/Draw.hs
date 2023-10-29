-- | This module defines how to turn
--   the game state into a picture
module Draw where

import Graphics.Gloss
import Types
import Board (fCordToLCord)

draw :: GameState -> IO Picture
draw (GameState {infoToShow = ShowABoard b}) = drawBoard =<< b
draw gs = return $ drawPure gs


drawPure :: GameState -> Picture
drawPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
    --pictures [color red (text "A"), translate 20 20 (color green (circle 10))]
  --ShowAChar   c -> color green (text [c])
    -- pictures $ fmap (pictures . map (\(MkField (x,y) ft) -> case ft of
--                                                                 Wall -> color green (text (show 0))
--                                     )) b
--    color green (text (show "9"))

drawBoard :: Board -> IO Picture
drawBoard b = return . pictures $ map (\(MkField c t) -> let (lx, ly) = fCordToLCord c 
                                                 in case t of
                                                    Wall -> translate lx ly (color red (text "a"))
                              ) (concat b)