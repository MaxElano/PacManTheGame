-- | This module defines how to turn
--   the game state into a picture
module Draw where

import Graphics.Gloss
import Types

draw :: GameState -> IO Picture
draw = return . drawPure

drawPure :: GameState -> Picture
drawPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowABoard  b -> pictures $ fmap (pictures . map (\(MkField (x,y) ft) -> case ft of
                                                                Wall -> color green (text (show 0))
                                    )) b
  --  color green (text (show "9"))