module RA
import StdEnv


:: Point = { x :: Int, y::Int }

origin :: Point
origin = {x=0, y=0}
//Start = origin

p1 :: Point
p1 = {x=3, y=4}
p2 :: Point
p2 = {x=4, y=5}
//Start = p1
distance :: Point Point->Real
distance a b = sqrt (toReal((x2-x1)^2 + (y2-y1)^2))
where
 x1 = a.x
 x2 = b.x
 y1 = a.y
 y2 = b.y
instance + Point
 where
 + a b = {x=a.x+b.x, y=a.y+b.y}
//Start = p1*p1*p1*p1*p1
instance zero Point 
 where 
  zero = {x=0,y=0}
instance == Point
 where
 == a b = a.x == b.x && a.y == b.y
//Start = isMember p2 [p1,p2,origin]
//Start = sum[p1,p2,origin]
:: Circle = {center ::Point, radius::Int}
Start = {center = origin, radius = 5}


