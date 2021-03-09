module Classes
import StdEnv

instance + String
where
 (+) s1 s2 = s1 +++ s2
//Start = "Hello " + "World"
instance + (a,b) | +a & +b
where
 (+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
//Start = (2,3) + (2,4)


increment n = n + 1
//Start = increment 4

double :: a -> a | +a
double a = a + a
//Start = double 3

delta :: a a a -> a | *,-,fromInt a
delta a b c = b*b - (fromInt 4)*a*c
//Start = delta 2 3 2

