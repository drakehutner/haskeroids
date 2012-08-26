-- ----------------------------------------
-- Haskeroids - Server
--
-- Author: Hanno Sternberg
-- ----------------------------------------

module Haskeroids where
import Display
import Datatypes
import System.Random


rndGen	= mkStdGen 1337

-- ----------------------------------------
-- 
rand 	:: Integer -> IO Integer
rand n	= do
  r <- randomIO 
  if n <= 0 then return 1 else return $ ((r `mod` n) +1)



-- ----------------------------------------
-- Generate a route with a number of points
generateRoute		:: Size -> Blockades -> Integer -> IO Route
generateRoute s bs n	= genRoute s bs n []
  where
    genRoute		:: Size -> Blockades -> Integer -> Route -> IO Route
    genRoute _ _ 0 r	= return r
    genRoute (w,h) bs n r	= do 
      rw <- rand w
      rh <- rand h
      if (isBlocked bs (rw,rh) 1) then genRoute (w,h) bs (n) r
      else genRoute (w,h) bs (n-1) ((rw,rh):r)
      
-- ----------------------------------------
-- Generates a number of blockades
-- Size: Maximum dimension
-- Size: Size of the Track
-- Integer: Number of Blockades
generateBlockades	:: Size -> Size -> Integer -> IO Blockades
generateBlockades d s n = genBlockade d s n []
  where 
    genBlockade 	:: Size -> Size -> Integer -> Blockades -> IO Blockades
    genBlockade _ _ 0 bs 		= return bs
    genBlockade (mx,my) (mw,mh) n bs 	= do
      w <- rand mw
      h <- rand mh
      x <- rand (mx - w)
      y <- rand (my - h)
      if (isBlocked bs (x,y) 2) then genBlockade (mw,mh) (mx,my) (n) bs
      else genBlockade (mw,mh) (mx,my) (n-1) (((x+1,y+1),(w+1,h+1)):bs)
      
-- ----------------------------------------
-- Generates a racetrack      
-- Size: Size of the map
-- Size: Maximum size of the blockades
-- Integer: Number of blockades
-- Integer: Number of waypoints
generateTrack		:: Size -> Size -> Integer -> Integer -> IO Track
generateTrack s m nb nr	= do
  b <- generateBlockades s m nb
  r <- generateRoute s b nr
  return (s,b,r,[])
  
main :: IO ()
main = do
  t <- generateTrack (30, 25) (5,5) 25 12
  putStrLn $ show t
  displayTrack t
  
  
  
  
  
  