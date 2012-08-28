-- ----------------------------------------
-- Haskeroids - main 
--
-- Author: Hanno Sternberg
-- ----------------------------------------


module Main where

import Datatypes
import Display
import Haskeroids
import NavManual
import NavAuto

 
-- ----------------------------------------
--   
play :: ControlFunc ->  IO ()
play f = do
  t <- generateTrack (10, 7) (2,2) 10 3
  t <- addShipToTrack t 1
  displayTrack t
  t <- f 1 t
  putStrLn ("Ships finished: " ++ (show $ shipsFinished t))
  displayTrack t
  