module Ghost (moveAllGhosts, findAllTargetFields, changeAllGhostColor) where
import Types as T
    ( TargetFieldCord,
      BaseField,
      GhostType(Orange, Red, Pink, Cyan),
      GhostLocation,
      Ghost(..),
      PacManLocation,
      PacMan(PacMan, pacManLocation),
      Orientation(..),
      LocationCord,
      Location(Location),
      FieldCord,
      FieldType(..),
      Field(MkField),
      Board,
      GhostMode(..),
      GhostHouseStatus(..),
      GameState(GameState, ghostOrange, ghostMode, generator, board,
                elapsedTime, ghostCyan, ghostPink, ghostRed, pacMan),
      ElapsedTime, Size, GhostColorTo (..), ghostDarkColor)
import Entity ( moveEntity, oppositeOrientation, boundaryCheck )
import Board
    ( locationToField, findFieldCordAhead, useRandom, lCordToFCord )
import System.Random ( StdGen )
import GHC.Real (mkRationalBase10)
import Graphics.Gloss.Data.Color as C
import GHC.RTS.Flags (DebugFlags(gc), getParFlags)
import Data.Maybe

--Main Function 1 for the entire module. Handles all movement for the ghosts
moveAllGhosts :: GameState -> GameState
moveAllGhosts gs = let (ngr, ng1) = moveGhost (board gs) (ghostRed    gs) (elapsedTime gs) (ghostMode gs) (generator gs)
                       (ngc, ng2) = moveGhost (board gs) (ghostCyan   gs) (elapsedTime gs) (ghostMode gs) ng1
                       (ngp, ng3) = moveGhost (board gs) (ghostPink   gs) (elapsedTime gs) (ghostMode gs) ng2
                       (ngo, ng4) = moveGhost (board gs) (ghostOrange gs) (elapsedTime gs) (ghostMode gs) ng3
                   in gs
                       { ghostRed    = ngr
                       , ghostCyan   = ngc
                       , ghostPink   = ngp
                       , ghostOrange = ngo
                       , generator   = ng4
                       }

--Handles all the movement for one ghost
moveGhost :: Board -> Ghost -> ElapsedTime -> GhostMode -> StdGen -> (Ghost, StdGen)
--Reverses the ghost if neccessary
moveGhost b g@(Ghost
    { ghostLocation = l@(Location c o)
    , targetField   = t
    , mustReverse   = True
    , ghostSpeed    = s
    , ghostSize     = z
    }) et _ gen     = (g 
        { ghostLocation = moveEntity (Location c (oppositeOrientation o)) s et z
        , mustReverse   = False 
        }, gen)
--Handles random direcion, when frightened
moveGhost b g@(Ghost
    { ghostLocation = l@(Location c _)
    , targetField   = t
    , ghostSpeed    = s
    , ghostSize     = z
    }) et Frightened gen = let (no, ng) = chooseRandomDirection gen (tryAllOrientations b l z)
                           in (g { ghostLocation = moveEntity (Location c no) s et z }, ng)
--Handles random direction, when inside of GhostHouse
moveGhost b g@(Ghost
    { ghostLocation    = l@(Location c _)
    , targetField      = t
    , ghostSpeed       = s
    , ghostSize        = z
    , ghostHouseStatus = Inside
    }) et _ gen = let (no, ng) = chooseRandomDirection gen (tryAllOrientations b l z)
                           in (g { ghostLocation = moveEntity (Location c no) s et z }, ng)
--"Normal" move
moveGhost b g@(Ghost
    { ghostLocation = l@(Location c _)
    , targetField   = t
    , ghostSpeed    = s
    , ghostSize     = z
    }) et _ gen     = (g { ghostLocation = moveEntity (Location c (findOrientation b l t z)) s et z }, gen)
        where
            --Main function for finding the new orientation for the ghost
            findOrientation :: Board -> GhostLocation -> TargetFieldCord -> Size -> Orientation
            findOrientation b gl@(Location gc oo) tf s = let ao = tryAllOrientations b gl s
                                                        in case ao of
                                                           [x] -> x
                                                           xs  -> fromMaybe oo $ checkEveryLocation b tf (lCordToFCord gc) xs
            --Checks which new field for the ghost is the closest to his TargetField
            checkEveryLocation :: Board -> TargetFieldCord -> FieldCord -> [Orientation] -> Maybe Orientation
            checkEveryLocation b t c [x1, x2] = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                    d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                                in if d1 <= d2
                                                    then Just x1
                                                    else Just x2
            checkEveryLocation b t c (x1:x2:xs) = let d1 = distanceFCToFloat t (findFieldCordAhead c x1 1)
                                                      d2 = distanceFCToFloat t (findFieldCordAhead c x2 1)
                                                  in if d1 <= d2
                                                    then checkEveryLocation b t c (x1:xs)
                                                    else checkEveryLocation b t c (x2:xs)
            checkEveryLocation b t c [x]       = Just x
            checkEveryLocation b t c []        = Nothing
            --Calculates the distance between two FieldCords in Float
            distanceFCToFloat :: FieldCord -> FieldCord -> Float
            distanceFCToFloat (px, py) (gx, gy) = sqrt (abs ((npx - ngx) * (npx - ngx) + (npy - ngy) * (npy - ngy)))
                where
                    npx = fromIntegral px :: Float
                    npy = fromIntegral py :: Float
                    ngx = fromIntegral gx :: Float
                    ngy = fromIntegral gy :: Float

--Chooses random direction from list
chooseRandomDirection :: StdGen -> [Orientation] -> (Orientation, StdGen)
chooseRandomDirection g xs = let (rn, ng) = useRandom g (0, length xs - 1)
                             in (xs !! rn, ng)

--Finds all allowed new orientations for the ghost
tryAllOrientations :: Board -> GhostLocation -> Size -> [Orientation]
tryAllOrientations b gl@(Location _ o) s = checkPossibility b gl (filter (\d -> d /= oppositeOrientation o ) [T.Up, T.Right, T.Down, T.Left]) s
    where
        checkPossibility :: Board -> GhostLocation -> [Orientation] -> Size -> [Orientation]
        checkPossibility b gl@(Location l@(x,y) o) [] s     = []
        checkPossibility b gl@(Location l@(x,y) o) (z:zs) s | boundaryCheck b l z s = checkPossibility b gl zs s
                                                            | otherwise = z : checkPossibility b gl zs s






--Main Function 2 for the entire module. Finds each target field and returns them inside the new GameState
findAllTargetFields :: GameState -> GameState
findAllTargetFields gs@(GameState { ghostMode = Chase }) = gs
    { ghostRed    = findTargetField (ghostRed    gs) gs
    , ghostCyan   = findTargetField (ghostCyan   gs) gs
    , ghostPink   = findTargetField (ghostPink   gs) gs
    , ghostOrange = findTargetField (ghostOrange gs) gs
    }
findAllTargetFields gs@(GameState { ghostMode = Scatter
                                  , ghostRed    = gr
                                  , ghostCyan   = gc
                                  , ghostPink   = gp
                                  , ghostOrange = go }) = gs
    { ghostRed    = gr {targetField = baseField gr}
    , ghostCyan   = gc {targetField = baseField gc}
    , ghostPink   = gp {targetField = baseField gp}
    , ghostOrange = go {targetField = baseField go}
    }
findAllTargetFields gs@(GameState { ghostMode = Frightened
                                  , ghostRed    = gr
                                  , ghostCyan   = gc
                                  , ghostPink   = gp
                                  , ghostOrange = go }) = gs
    { ghostRed    = gr {targetField = baseField gr}
    , ghostCyan   = gc {targetField = baseField gc}
    , ghostPink   = gp {targetField = baseField gp}
    , ghostOrange = go {targetField = baseField go}
    }

--Decides which algorithm to use to chase PacMan, depends on ghostType
findTargetField :: Ghost -> GameState -> Ghost
findTargetField g@(Ghost { ghostType = Red })    (GameState { pacMan      = (PacMan { pacManLocation = pl }) }) = g { targetField = findTargetFieldRed pl }
findTargetField g@(Ghost { ghostType = Pink })   (GameState { pacMan      = (PacMan { pacManLocation = pl }) }) = g { targetField = findTargetFieldPink pl }
findTargetField g@(Ghost { ghostType = Cyan })   (GameState
    { pacMan      = (PacMan { pacManLocation = pl })
    , ghostRed    = (Ghost  { ghostLocation = gl })
    }) = g { targetField = findTargetFieldCyan pl gl }
findTargetField g@(Ghost { ghostType = Orange }) (GameState
    { pacMan      = (PacMan {pacManLocation = pl})
    , ghostOrange = (Ghost
        { ghostLocation = gl
        , baseField = bf
        })
    }) = g {targetField = findTargetFieldOrange pl gl bf}

--(Red Ghost) TargetField is PacMan's location -> field
findTargetFieldRed :: PacManLocation -> TargetFieldCord
findTargetFieldRed (Location p _) = lCordToFCord p

--(Pink Ghost) TargetField is 4 fields ahead of PacMan
findTargetFieldPink :: PacManLocation -> TargetFieldCord
findTargetFieldPink (Location p o) = findFieldCordAhead (lCordToFCord p) o 4

--(Cyan Ghost) TargetField is the field mirrored to the red ghost's location from 2 ahead of PacMan
findTargetFieldCyan :: PacManLocation -> GhostLocation -> TargetFieldCord
findTargetFieldCyan (Location p o) (Location g _) = let (npx, npy) = findFieldCordAhead (lCordToFCord p) o 2
                                                        (ngx, ngy) = lCordToFCord g
                                                    in (npx * 2 - ngx , npy * 2 - ngy)

--(Orange Ghost) Uses it's own location and pacman's location, if within 8 range -> back to base, otherwise use red algorithm
findTargetFieldOrange :: PacManLocation -> GhostLocation -> BaseField -> TargetFieldCord
findTargetFieldOrange pl@(Location pc o) (Location gc _) bf | distance pc gc > 8 = findTargetFieldRed pl
                                                            | otherwise          = bf
    where
        distance :: LocationCord -> LocationCord -> Float
        distance (px, py) (gx, gy) = sqrt (abs ((px - gx) * (px - gx) + (py - gy) * (py - gy)))




changeAllGhostColor :: GameState-> GhostColorTo -> GameState
changeAllGhostColor gs c = gs { ghostRed    = changeGhostColor (ghostRed gs)  c
                              , ghostPink   = changeGhostColor (ghostPink gs) c
                              , ghostCyan   = changeGhostColor (ghostCyan gs) c
                              , ghostOrange = changeGhostColor (ghostOrange gs) c }

changeGhostColor :: Ghost -> GhostColorTo -> Ghost
changeGhostColor g T.Normal = g {ghostColor = ghostBaseColor g}
changeGhostColor g T.Dark   = g {ghostColor = ghostDarkColor}
