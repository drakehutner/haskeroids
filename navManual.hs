-- ----------------------------------------
-- Haskeroids - Manual ship control
--
-- Author: Hanno Sternberg
-- ----------------------------------------


module NavManual where

import Datatypes
import Display
import Haskeroids
  
-- ----------------------------------------
--   
manual 	:: ControlFunc
manual id t	= do
  putStrLn ("Player " ++ (show id))
  case s of
    Nothing	-> putStrLn "No Ship found"
    Just s'	-> putStrLn ("Remaining Waypoints: " ++ (show $ getShipRoute s'))
  d <- readDirection
  t <- moveShip t id d
  displayTrack t
  if (elem  id $ shipsFinished t) then return t else manual id t
  where
    s = getShip 1 $ getShips t