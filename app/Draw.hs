-- | This module defines how to turn
--   the game state into a picture
module Draw where

import Graphics.Gloss
    ( green,
      red,
      white,
      yellow,
      Picture,
      blank,
      circle,
      circleSolid,
      color,
      pictures,
      scale,
      text,
      translate )
import Types as T
    ( fieldSize,
      Board,
      Field(MkField),
      FieldType(..),
      GameState(..),
      InfoToShow(ShowABoard, ShowNothing, ShowANumber, ShowAChar, ShowPlayState, ShowAPosition), PacMan (..), Location (Location), Ghost (..), GhostType (..), Score, windowSize, Orientation (..) )
import Board (fCordToLCord, locationToField)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

draw :: GameState -> IO Picture
draw gs = return $ drawPure gs


drawPure :: GameState -> Picture
drawPure gs = case infoToShow gs of
  ShowNothing     -> blank
  ShowANumber n   -> color green (text (show n))
  ShowAChar   c   -> color green (text [c])
  ShowPlayState   -> drawPlayState gs
  ShowAPosition l -> let f = locationToField l (board gs)
                     in color white $ translate (-200) 0 (scale 0.2 0.2 (text (show l ++ show f)))

drawPlayState :: GameState -> Picture
drawPlayState gs = let (x,y,p) = drawBoard (board gs)
                       (wx, wy) = windowSize
                       sc = min (fromIntegral wx / x) (fromIntegral wy / (y + fromIntegral fieldSize / 2))
                   in scale sc sc $ translate ((-x - fromIntegral fieldSize / 2) / 2) ((-y - fromIntegral fieldSize / 2) / 2) $ pictures [p, drawPacMan (pacMan gs), drawAllGhosts gs, drawScore (score gs)]



drawBoard :: Board -> (Float, Float, Picture)
drawBoard b = let ls = helpDrawBoard
                  x = maximum (map (fst . fst) ls)
                  y = maximum (map (snd . fst) ls)
                  p = map snd ls
              in (x, y, pictures p)
    where
        helpDrawBoard = map (\(MkField c t) -> let (lx, ly) = fCordToLCord c
                                                   z = fromIntegral fieldSize
                                                 in case t of
                                                    Wall -> ((lx, ly), translate lx ly (color blue (polygon [(-z/2,-z/2), (-z/2,z/2), (z/2,z/2), (z/2,-z/2)])))
                                                    Pellet -> ((lx, ly), translate lx ly (color yellow (circleSolid $ z / 4)))
                                                    _    -> ((lx, ly), blank)
                             ) (concat b)

drawPacMan :: PacMan -> Picture
drawPacMan (PacMan { pacManLocation = (Location (x,y) T.Up   )
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate (-90) (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Right)
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 0     (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Down ) 
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 90    (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Left ) 
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 180   (color yellow (thickArc ma pa r t))

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