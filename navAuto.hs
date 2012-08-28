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
-- calculates the next position
calcNextMove            :: Ship -> Position -> Track -> Direction
calcNextMove s r t =
  if ((h == -1) && (not $ isBlocked (getBlockades t) (moveObject p LEFT) 0)) then LEFT
  else if ((h == 1) && (not $ isBlocked (getBlockades t) (moveObject p RIGHT) 0)) then RIGHT
  else if ((v == -1) && (not $ isBlocked (getBlockades t) (moveObject p UP) 0)) then UP
  else if ((v == 1) && (not $ isBlocked (getBlockades t) (moveObject p DOWN) 0)) then DOWN
  else STOP
  where
    p = getShipPosition s
    h = if (fst p == fst r) then 0 else if (fst p > fst r) then (-1) else 1
    v = if (snd p == snd r) then 0 else if (snd p > snd r) then (-1) else 1
    

-- ----------------------------------------
-- navigates a route
navigate	:: Route -> ControlFunc
naviagte [] id t
  = return t
navigate (r:rs) id t
  = do
    displayTrack t
    if (d == STOP) then return t
    else do
      t <- moveShip t id d
      if (r == (getShipPosition $ getShipNull id $ getShips t)) then
        navigate rs id t
      else
        navigate (r:rs) id t
    where
      d = calcNextMove s r t
      s = getShipNull id $ getShips t




-- ----------------------------------------
--
autoNav 	:: ControlFunc
autoNav id t
  = navigate (prepareRoute (getShipRoute s) (getShipPosition s)) id t
    where
      s = getShipNull id $ getShips t


