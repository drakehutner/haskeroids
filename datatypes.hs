-- ----------------------------------------
-- Haskeroids - Datatypes
-- 
-- Author: Hanno Sternberg
-- ----------------------------------------

module Datatypes where

import Control.Exception 

-- ----------------------------------------
-- Base types
type Coordinate 	= Integer
type Dimension		= Integer

-- ----------------------------------------
-- Position: Pair of coordinate
type Position		= (Coordinate, Coordinate)
type Positions		= [Position]

-- ----------------------------------------
-- Size: Pair of dimension
type Size		= (Dimension, Dimension)
type Sizes		= [Size]

-- ----------------------------------------
-- Track is just a synonym for a list of position
type Route		= Positions

-- ----------------------------------------
-- A ship has an owner and a position
type ShipID		= Integer
type Ship		= (ShipID, Position, Route)
type Ships		= [Ship]

-- ----------------------------------------
-- Blockade: Defined by a position and a size
type Blockade		= (Position, Size)
type Blockades		= [Blockade]

-- ----------------------------------------
-- Size: 	Size of the complete map
-- Route: 	Route of the race
-- Blockades: 	List of blocking elements 
-- Ships:	List of participants
type Track		= (Size, Blockades, Route, Ships)

-- ----------------------------------------
-- 
data Direction		= STOP | UP | RIGHT | DOWN | LEFT
  deriving (Show, Read, Eq, Enum)

-- ----------------------------------------
-- 
type ControlFunc 	= ShipID -> Track -> IO Track  
  
-- ----------------------------------------
-- 
readDirection	:: IO Direction
readDirection	= do
  putStr ("enter direction " ++ (show [STOP, UP, RIGHT, DOWN, LEFT]) ++ ": ") 
  raw <- try getLine :: IO (Either IOException String)
  putStr "\n\n"
  case raw of 
    Left _ -> do  -- Exception
      putStrLn ("Error")
      readDirection
    Right rawStr -> do -- read string
      dir <- try $ evaluate $ read rawStr :: IO (Either ErrorCall Direction)
      case dir of
        Left _ -> do
          putStrLn "Invalid direction! "
          readDirection
        Right d -> do
          return d

-- ----------------------------------------
-- Accessing the shiplist of a track
getShips		:: Track -> Ships
getShips (_,_,_,s)	= s

-- ----------------------------------------
-- Searching a ship inside a shiplist
getShip			:: Ships -> ShipID -> Maybe Ship
getShip [] i		= Nothing
getShip ((id,p,r):ss) i	= if (i == id) then Just (id,p,r) else getShip ss i

-- ----------------------------------------
-- Gets the remaining waypoints from a ship
getShipRoute		:: Ship -> Route
getShipRoute (_,_,r)	= r

-- ----------------------------------------
-- Get the position of a ship
getShipPosition		:: Ship -> Position
getShipPosition (_,p,_)	= p

-- ----------------------------------------
--   
moveObject 		:: Position -> Direction -> Position
moveObject (x,y) d	= 
  case d of
    STOP 	-> (x,y)
    UP		-> (x,y-1)
    RIGHT	-> (x+1,y)
    DOWN	-> (x,y+1)
    LEFT	-> (x-1,y)

-- ----------------------------------------
--   
shipsFinished		:: Track -> [ShipID]
shipsFinished t 		= shipsFinished' [] $ getShips t
  where
    shipsFinished' 	:: [ShipID] -> Ships -> [ShipID]
    shipsFinished' l []		= l
    shipsFinished' l ((id,_,r):ss)	
      = if (null r) then shipsFinished' (id:l) ss else shipsFinished' l ss
    
-- ----------------------------------------
-- 
isShip		:: Ships -> Position -> [ShipID]
isShip s p	= isShip' s p []
  where 
    isShip' 	:: Ships -> Position -> [ShipID] -> [ShipID]
    isShip' []_ l				
      = l
    isShip' ((n,(sx,sy),_):ss) (x,y) l	
      = if ((sx == x) && (sy == y)) then isShip' ss (x,y) (n:l)
        else isShip' ss (x,y) l
	
-- ----------------------------------------
-- 
isBlocked		:: Blockades -> Position -> Integer -> Bool
isBlocked [] _ _	
  = False
isBlocked (((bx,by),(bw,bh)):bs) (x,y) b
  = if (((bx >= x) && (bx < x+bw+b)) && ((by >= y) && (by < y+bh+b) )) then True
    else isBlocked bs (x,y) b
    
-- ----------------------------------------
-- 
isRoute			:: Route -> Position -> Bool
isRoute [] _
  = False
isRoute ((rx,ry):rs) (x,y)
  = if ((rx == x) && (ry == y)) then True
    else isRoute rs (x,y)
    
-- ----------------------------------------
-- 
deleteElem		:: Eq a => [a] -> a -> [a]
deleteElem [] e		= []
deleteElem (x:xs) e	= if (x == e) then deleteElem xs e else x : deleteElem xs e












