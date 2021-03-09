module small
import StdEnv
/*
Grade 2 : Given a real number, find if it is negative
Grade 3 : Given a list of real numbers, remove all negative numbers 
Grade 4 : Given a list of real numbers, return list which contains square root of all non-negative numbers
Grade 5 : Given a number, find roots from all positive numbers which are multiple of 3 and less or equal to the given number.
*/
 
//isNegative:: Real -> Bool
isNegative n
|n<0 = True
= False
Start = isNegative 1.2 // False
// Start = isNegative -0.3 // True 
// Start = isNegative 0 // False
 
//remNegatives:: [Real] -> [Real]
 
// Start = remNegatives [1.0,2.5,-3.2,-1.5,3.0] // [1.0,2.5,3.0]
// Start = remNegatives [-1.0,-0.5,-3.4] // []
// Start = remNegatives [0.0,2.5,-1.3] // [0.0,2.5]
 
//getRoots:: [Real] -> [Real]
 
// Start = getRoots [25.0,-3.4,1.0] // [5.0,1.0]
// Start = getRoots [-1.0,-0.5,-3.4] // []
// Start = getRoots [25.0, 16.0, 4.0, 100.0] // [5.0, 4.0, 2.0, 10.0]
 
//getRootsWhich:: Int -> [Real]
 
// Start = getRootsWhich 10 // [1.73205080757,2.44948974278,3]
// Start = getRootsWhich 2 // []
// Start = getRootsWhich 21 // [1.73205080757,2.44948974278,3,3.46410161514,3.87298334621,4.24264068712,4.58257569496]