-- | This module defines how to turn
--   the game state into a picture
module Draw where

import Graphics.Gloss
import Types
import Board (fCordToLCord)

draw :: GameState -> IO Picture
draw gs = return $ drawPure gs


drawPure :: GameState -> Picture
drawPure gs = case infoToShow gs of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowPlayState -> drawPlayState gs
    --pictures [color red (text "A"), translate 20 20 (color green (circle 10))]
  --ShowAChar   c -> color green (text [c])
    -- pictures $ fmap (pictures . map (\(MkField (x,y) ft) -> case ft of
--                                                                 Wall -> color green (text (show 0))
--                                     )) b
--    color green (text (show "9"))

drawPlayState :: GameState -> Picture
drawPlayState gs = scale 2 2 $ pictures [drawBoard (board gs), drawPacMan (pacMan gs), drawAllGhosts gs, drawScore (score gs)]

drawBoard :: Board -> Picture
drawBoard b = pictures $ map (\(MkField c t) -> let (lx, ly) = fCordToLCord c 
                                                 in case t of
                                                    Wall -> translate lx ly (color red (circle (fromIntegral fieldSize / 2)))
                             ) (concat b)

drawPacMan :: PacMan -> Picture
drawPacMan (PacMan {pacManSize = s 
                   ,pacManLocation = (Location (x,y) _)}) = translate x y (color yellow (circleSolid (fromIntegral s / 2)))

drawAllGhosts :: GameState -> Picture
drawAllGhosts gs = pictures [drawGhost (ghostRed gs), drawGhost (ghostPink gs), drawGhost (ghostCyan gs), drawGhost (ghostOrange gs)]

drawGhost :: Ghost -> Picture
drawGhost g@(Ghost {ghostType = Red    
                   ,ghostLocation = (Location (x,y) _)}) = translate x y (color (ghostColor g) (circleSolid (fromIntegral (ghostSize g) / 2)))
drawGhost g@(Ghost {ghostType = Pink
                   ,ghostLocation = (Location (x,y) _)}) = translate x y (color (ghostColor g) (circleSolid (fromIntegral (ghostSize g) / 2)))
drawGhost g@(Ghost {ghostType = Cyan
                   ,ghostLocation = (Location (x,y) _)}) = translate x y (color (ghostColor g) (circleSolid (fromIntegral (ghostSize g) / 2)))
drawGhost g@(Ghost {ghostType = Orange
                   ,ghostLocation = (Location (x,y) _)}) = translate x y (color (ghostColor g) (circleSolid (fromIntegral (ghostSize g) / 2)))

drawScore :: Score -> Picture   --Still needs to be translated
drawScore s = color white (text (show s))

