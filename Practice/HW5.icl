module HW5
import StdEnv

// find the sum of all odd squares that are smaller than 10,000
f1 :: Int

f1 = sum(takeWhile((>)10000)[x^2\\x<-[1..10000] | isOdd x]) 
//| x<>100 = abort"Error"
 
Start = f1 // 166650



// Given list of integers, find number of
// different sums of continuous subsequences
// Example: [1,2,3,4] has 10 sunsquence
// [1], [2], [1,2], [3], [2,3], [1,2,3], [4], [3,4], [2,3,4], [1,2,3,4]
// And there sums are [1,2,3,2,4,5,4,6,8,9], from which we need to remove
// duplicates and we get [1,2,3,4,5,6,8,9] - Hence we ave 8 different sums


// f2 :: [Int] -> Int



exists :: Int [Int] -> Bool
exists n [] = False
exists n [x:xs] = n == x || exists n xs 

existsMain :: [Int] -> Bool
existsMain [] = False 
existsMain [n] = False 
existsMain [x:xs] = exists x xs || existsMain xs

ExistDups :: [[Int]] -> [[Int]]
ExistDups [] = []
ExistDups [firstList: restLists]
| existsMain firstList = ExistDups restLists 
= [firstList : ExistDups restLists]

AuxSqGen :: [Int] -> [[Int]] 
AuxSqGen [] = []
AuxSqGen [n] = [[n]]
AuxSqGen list = [list] ++ AuxSqGen (init list)

sqGenerator :: [Int] -> [[Int]]
sqGenerator [] = []
sqGenerator list =  length(ExistDups(AuxSqGen list ++ sqGenerator (tl list)))




//Start = sqGenerator [1,2,3,4] // 9
//Start =  sqGenerator [] // 0
//Start = f2 [3] // 1
//Start = sqGenerator [1,-3,2,-4,-3,1,7,6,2,8,9] // 34
//Start = sqGenerator [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // 166

// Given the list of integers. Find the longest
// continues subsequence which does not have
// duplicates. If there are several answers
// return rightmost one.

// f3:: [Int] -> [Int]

exists :: Int [Int] -> Bool
exists n [] = False
exists n [x:xs] = n == x || exists n xs 

existsMain :: [Int] -> Bool
existsMain [] = False 
existsMain [n] = False 
existsMain [x:xs] = exists x xs || existsMain xs

ExistDups :: [[Int]] -> [[Int]]
ExistDups [] = []
ExistDups [firstList: restLists]
| existsMain firstList = ExistDups restLists 
= [firstList : ExistDups restLists]

AuxSqGen :: [Int] -> [[Int]] 
AuxSqGen [] = []
AuxSqGen [n] = [[n]]
AuxSqGen list = [list] ++ AuxSqGen (init list)

abcd :: 
abcd [x] = x
abcd [x:xs]
| lnX > h = abcd ([x] ++ tail)
| lnX < h = abcd ([hd(xs)]++ tail)
= abcd ([hd(xs)] ++ tail)
where 
	lnX = length x
	h = length (hd(xs))
	tail = tl(xs)

sqGenerator :: [Int] -> [[Int]]
sqGenerator [] = []
sqGenerator list =  ExistDups(AuxSqGen list ++ sqGenerator (tl list))

f3 :: [Int] -> [Int]
f3 list = abcd (sqGenerator list)


///Start = removeDuplicates [1,1,2,4,5,3,2,6,3] // [4,5,3,2,6]
// Start = f3 [] // []
// Start = f3 [3] // [3]
// Start = f3 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]
