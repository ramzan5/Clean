module Home5
import StdEnv

// find the sum of all odd squares that are smaller than 10,000
f1 :: Int

f1 = sum(takeWhile((>)10000)[x^2\\x<-[1..10000] | isOdd x]) 
 
//Start = f1 // 166650

// Given list of integers, find number of
// different sums of continuous subsequences
// Example: [1,2,3,4] has 10 sunsquence
// [1], [2], [1,2], [3], [2,3], [1,2,3], [4], [3,4], [2,3,4], [1,2,3,4]
// And there sums are [1,2,3,2,4,5,4,6,8,9], from which we need to remove
// duplicates and we get [1,2,3,4,5,6,8,9] - Hence we ave 8 different sums
/*
f2 :: [Int] -> [Int]  
f2 [] = []
f2 [n] = [n]
f2 [x : y] 
| x == hd y =  f2 y  
= [x] ++ f2 y
*/
f3 :: [Int] -> [Int]
f3 [] = []
f3 list = [sum list] ++ f3 (init list)  

f4 :: [Int] -> [Int]
f4 [] = []
f4 list = (f3 list ++ f4(tl list))

f5 :: [Int] -> Int
f5 [] = 0
f5 list = length(removeDup(f4 list))

//Start = f5 [1,2,3,4] // 9
//Start = f5 [] // 0
//Start = f5 [3] // 1
//Start = f5 [1,-3,2,-4,-3,1,7,6,2,8,9] // 34
//Start = f5 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // 166



// Given the list of integers. Find the longest
// continues subsequence which does not have
// duplicates. If there are several answers
// return rightmost one.



exist :: Int [Int] -> Bool
exist n [] = False
exist n [x:y] = n == x || exist n y 

exists :: [Int] -> Bool
exists [] = False 
exists [n] = False 
exists [x:y] = exist x y || exists y

ExistC :: [[Int]] -> [[Int]]
ExistC [] = []
ExistC [firstList: restLists]
| exists firstList = ExistC restLists 
= [firstList : ExistC restLists]

SqGen :: [Int] -> [[Int]] 
SqGen [] = []
SqGen [n] = [[n]]
SqGen list = [list] ++ SqGen (init list)

abcd [x] = x
abcd [x:y]
| lnX > h = abcd ([x] ++ tail)
| lnX < h = abcd ([hd(y)]++ tail)
= abcd ([hd(y)] ++ tail)
where 
	lnX = length x
	h = length (hd(y))
	tail = tl(y)

SqGenerate :: [Int] -> [[Int]]
SqGenerate [] = []
SqGenerate list =  ExistC(SqGen list ++ SqGenerate (tl list))

f6 :: [Int] -> [Int]
f6 [] = []
f6 list = abcd (SqGenerate list)



//Start = f6 [] // []
//Start = f6 [3] // [3]
//Start = f6 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]










