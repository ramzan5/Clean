module TASK6
import StdEnv

/*
Given a list of Tuples of Integer, give a list of integers produced if we raise
the first integer to the power of the second integer and keep only the even numbers.
Example :
[(2,4),(3,2)] --->[16]
because (2,4) ->2^4 = 16 and is even
(3,2) -> 3^2=9 is odd
*/
power :: [(Int,Int)] ->[Int]
power [] = []
power [(a,b):y]
|isEven (a^b) = [a^b:power y]
= power y 

//Start = power [(2,4),(3,2)]//[16]
//Start=power [(1,100),(2,3),(4,5)]//[8,1024]
//Start = power [(3,5),(7,9)]//[]