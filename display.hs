-- ----------------------------------------
-- Haskeroids - Server
--
-- Author: Hanno Sternberg
-- ----------------------------------------

module Display where
import Datatypes

-- ----------------------------------------
-- Displays a ship
displayShip 		:: ShipID -> IO ()
displayShip id		=
  if (id < 10) then putStr (" " ++ (show id) ++ " ")
  else  putStr (" " ++ (show id))

    
-- ----------------------------------------
-- Displays a single field
displayField			:: Track -> Position -> IO ()
displayField (_,b,r,s) p
  = do
    if (isBlocked b p 0) then putStr "@@@"
    else if (isRoute r p) then putStr " # "
      else do
	if (not $ null shs) then displayShip $ head shs
        else putStr " . "
	where shs = isShip s p

-- ----------------------------------------
-- Displays a line of the map
displayLine		:: Track -> Coordinate -> IO ()
displayLine t y		= 
  do
  putStr "\n  |"
  displayLine' t y 0
  where
    displayLine' 	:: Track -> Coordinate -> Integer -> IO ()
    displayLine' t@((w,h),b,r,s) y x
      = do 
	displayField t (x,y)
        if (x < w) then displayLine' t y (x+1)
	else putStr "|"

-- ----------------------------------------
-- Displays the map border
displayBorder		:: Dimension -> IO ()
displayBorder w		= do
  putStr "  +"
  displayBorder' w 0
  putStr "+"
  where
    displayBorder' 	:: Dimension -> Dimension -> IO ()
    displayBorder' w x	=  
      if (x <= w) then do 
        putStr "---" 
        displayBorder' w (x+1)
      else putStr ""
	
-- ----------------------------------------
-- Displays the whole racetrack
displayTrack		:: Track -> IO ()
displayTrack ((w,h),b,r,s)	= do
  putStrLn "\n"
  displayBorder w
  displayTrack' ((w,h),b,r,s) 0
  displayBorder w
  putStrLn "\n"
  where
    displayTrack' 	:: Track -> Coordinate ->  IO ()
    displayTrack' t@((w,h),b,r,s) y	
      = do 
        displayLine t y
        if (y < h) then displayTrack' ((w,h),b,r,s) (y+1)
	else putStrLn ""
	
