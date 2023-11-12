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
      InfoToShow(..), 
      PacMan (..), 
      Location (Location), 
      Ghost (..), 
      GhostType (..), 
      Lives (..),
      Score, 
      windowSize, 
      Orientation (..) )
import Board (fCordToLCord, locationToField)
import Graphics.Gloss.Data.Picture
    ( blank,
      circleSolid,
      color,
      line,
      pictures,
      polygon,
      rotate,
      scale,
      text,
      thickArc,
      translate,
      Picture )
import Graphics.Gloss.Data.Color
    ( blue, green, red, white, yellow )
import Data.List (intercalate)

-- converts the game state to io picture
draw :: GameState -> IO Picture
draw gs = return $ drawPure gs

-- converts pure aspects to picture
drawPure :: GameState -> Picture
drawPure gs = case infoToShow gs of
  ShowAChar   c   -> color green (text [c])
  ShowPlayState   -> drawPlayState gs
  ShowPauseState  -> drawPauseState gs
  ShowWinState -> drawWinState $ score gs
  ShowLostState -> drawLostState $ score gs

-- draws the winstate
drawWinState :: Score -> Picture
drawWinState s = color white $ pictures [translate (-330) 0 $ scale 0.4 0.4 $ text ("You WON With " ++ show s ++ " Points!!"),translate (-330) 100 $ scale 0.4 0.4 $ text "Press space to continue"]

-- draws the lost state
drawLostState :: Score -> Picture
drawLostState s = color white $ pictures [translate (-330) 0 $ scale 0.4 0.4 $ text ("You Lost With " ++ show s ++ " Points.."),translate (-330) 100 $ scale 0.4 0.4 $ text "Press space to continue"]

-- draws the pause state
drawPauseState :: GameState -> Picture
drawPauseState gs = let t = color white $ translate (-200) 0 (scale 0.2 0.2 $ text "Game Paused, Press 'P' To Continue")
                    in pictures [drawPlayState gs, t]

-- draws the play state
drawPlayState :: GameState -> Picture
drawPlayState gs = let (x,y,p) = drawBoard (board gs)
                       (wx, wy) = windowSize
                       sc = min (fromIntegral wx / x) (fromIntegral wy / (y + fromIntegral fieldSize / 2))
                   in scale sc sc 
                    $ translate ((-x - fromIntegral fieldSize / 2) / 2) ((-y - fromIntegral fieldSize / 2) / 2) 
                    $ pictures [drawPacManLives (lives $ pacMan gs) y, p, drawPacMan (pacMan gs), drawAllGhosts gs, drawScore (score gs) y]

-- draws the board
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
                                                    Wall      -> ((lx, ly), translate lx ly (color blue (polygon [(-z/2,-z/2), (-z/2,z/2), (z/2,z/2), (z/2,-z/2)])))
                                                    GhostWall -> ((lx, ly), translate lx ly (color white (polygon [(-z/2,-z/4), (-z/2,z/4), (z/2,z/4), (z/2,-z/4)])))
                                                    Pellet    -> ((lx, ly), translate lx ly (color yellow (circleSolid $ z / 8)))
                                                    Cherry    -> ((lx, ly), translate lx ly (pictures [ color green (line [(-1.5, -1), (-1, 0), (0.3, 2)])
                                                                                                       ,color green (line [(1.5, -1), (1, 0), (0.3, 2.5)])
                                                                                                       ,translate (-1.5) (-1) (color red (circleSolid $ z / 6))
                                                                                                       ,translate  1.5   (-1) (color red (circleSolid $ z / 6))
                                                                                                      ]))
                                                    PowerUp   -> ((lx, ly), translate lx ly (color yellow (circleSolid $ z / 3)))
                                                    _         -> ((lx, ly), blank)
                             ) (concat b)

-- draws pac-man
drawPacMan :: PacMan -> Picture
drawPacMan (PacMan { pacManLocation = (Location (x,y) T.Up   )
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate (-90) (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Right)
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 0     (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Down )
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 90    (color yellow (thickArc ma pa r t))
drawPacMan (PacMan {pacManLocation = (Location (x,y) T.Left )
                , pacManPictureValues = (ma,pa,r,t)})       = translate x y $ rotate 180   (color yellow (thickArc ma pa r t))

-- draws all ghosts
drawAllGhosts :: GameState -> Picture
drawAllGhosts gs = pictures [drawGhost (ghostRed gs), drawGhost (ghostPink gs), drawGhost (ghostCyan gs), drawGhost (ghostOrange gs)]

-- draws the given ghost
drawGhost :: Ghost -> Picture
drawGhost g@(Ghost {ghostLocation = (Location (x,y) _)}) = translate x y (color (ghostColor g) (circleSolid (fromIntegral (ghostSize g) / 2)))

-- draws the score
drawScore :: Score -> Float -> Picture
drawScore s y = translate (-45) (y - 30) $ scale 0.1 0.1 $ color white (text (show s))

-- draws pac-mans lives
drawPacManLives :: Lives -> Float -> Picture 
drawPacManLives (Lives l) y = let life = color yellow (thickArc (-30) (30) 2 4)
                              in translate (225) (y - 10) $ pictures $ map (\n -> translate 0 ((-10) * n) life) (take (fromIntegral l) [0..])