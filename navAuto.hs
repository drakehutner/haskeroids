-- ----------------------------------------
-- Haskeroids - Auto Navigation
--
-- Author: Hanno Sternberg
-- ----------------------------------------


module NavAuto where

import Datatypes
import Display
import Haskeroids

-- ----------------------------------------
--
prepareRoute		:: Route -> Position -> Route
prepareRoute r p 	= prepRoute p (-1,-1) (-1) r r []
  where
    --                        Ship     best Pos    dist       cur      rem      akku
    prepRoute 		:: Position -> Position -> Integer -> Route -> Route -> Route -> Route
    prepRoute _ _ _ _ [] a = reverse a
    prepRoute p b m [] rr a
      = prepRoute b b (-1) rr' rr' (b:a)
        where
	  rr' = deleteElem b rr
    prepRoute p b m (r:rs) rr a	
      = if (d < m || m == -1) then prepRoute p r d rs rr a else prepRoute p b m rs rr a
      where
        d = distance p r

-- ----------------------------------------
-- 
get90DegDirections	:: Direction -> (Direction, Direction)
get90DegDirections d	= 
  case d of 
    LEFT	-> (UP, DOWN)
    DOWN	-> (LEFT, RIGHT)
    RIGHT 	-> (DOWN, UP)
    UP		-> (RIGHT, LEFT)
    STOP	-> (STOP, STOP)

-- ----------------------------------------
-- 
getOpposite	:: Direction -> Direction
getOpposite d	= 
  case d of
    LEFT 	-> RIGHT
    UP		-> DOWN
    RIGHT	-> LEFT
    DOWN	-> UP
    STOP	-> STOP
    
    
	
-- ----------------------------------------
-- Evade obstacles
findAlternative		:: Position -> Track -> Direction -> Direction
findAlternative p t d 	= 
  if (not $ isBlocked (getBlockades t) (moveObject p l) 0) then l
  else if (not $ isBlocked (getBlockades t) (moveObject p r) 0) then r
  else getOpposite d   
  where
    (l, r) = get90DegDirections d
  
	
-- ----------------------------------------
-- calculates the next position
calcNextMove            :: Ship -> Position -> Track -> Direction -> Direction
calcNextMove s r t d =
  if (h == -1 && d /= RIGHT) then LEFT
  else if (h == 1 && d /= LEFT) then RIGHT
  else if (v == -1 && d /= DOWN) then UP
  else if (v == 1 && d /= UP) then DOWN
  else STOP
  where
    p = getShipPosition s
    h = if (fst p == fst r) then 0 else if (fst p > fst r) then (-1) else 1
    v = if (snd p == snd r) then 0 else if (snd p > snd r) then (-1) else 1
    

-- ----------------------------------------
-- navigates a route
navigate	:: Route -> Direction -> ControlFunc
navigate [] _ _ t
  = return t
navigate (r:rs) oldD id t
  = do
    displayTrack t
    print (r:rs)
    getChar
    if (d' == STOP) then return t
    else do
      t <- moveShip t id d'
      if (r == (getShipPosition $ getShipNull id $ getShips t)) then
        navigate rs d' id t
      else
        navigate (r:rs) d' id t
    where
      p = getShipPosition $ getShipNull id $ getShips t
      d = calcNextMove s r t oldD
      d' = if ((isBlocked (getBlockades t) (moveObject p d) 0) || (d == STOP)) then findAlternative p t d else d
      s = getShipNull id $ getShips t




-- ----------------------------------------
--
autoNav 	:: ControlFunc
autoNav id t
  = navigate (prepareRoute (getShipRoute s) (getShipPosition s)) STOP id t
    where
      s = getShipNull id $ getShips t


