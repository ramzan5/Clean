module mid2
import StdEnv

/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

//SeqCheck :: [Int] -> Bool
SeqCheck :: [Int] -> Bool
SeqCheck [] = False
SeqCheck list = and[isOdd (x+y) \\x<-list & y<-[0..]]

//Start = SeqCheck [1..10] //True

//Start = SeqCheck [1,2,3] //True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */

//checkEven :: [Int] -> Bool
checkEven [] = False
checkEven list = isEven (length list)

//Start = checkEven [1,1,2,2,2,2,3,5,3,5] // True

//Start = checkEven [1,1,2,2,1] // False

//Start = checkEven [] //False

/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

//DotProd :: [Int] [Int] -> Int


DotProd list1 list2 = sum[x*y \\ x<-list1 & y<-list2]

//Start = DotProd [4,6,3] [6,3,7] //63

//Start = DotProd [6,3,7] [4,6,3] //63

//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0


// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. */

//TwoLists :: [Char] -> ([Char], [Char])

TwoLists :: [Char] -> ([Char], [Char])
//TwoLists myList = (filter (\char = char >= '0' && char <= '9') myList, filter (\char = char >= 'a' && char <= 'z') myList)
TwoLists list = (filter (\x = x >= '0' && x <= '9') list ,filter(\x = x >= 'a' && x <= 'z') list) 
//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

//Start = TwoLists [] // ([],[])
//Given a list of lists, for each list, extract the first, middle and last element. */

//Points3 :: [[Int]] -> [(Int, Int, Int)]

points3 list = [hd list] ++ [(list!!((length list)/2))] ++ [last list]
points4 [[]] = []
points4 list = [points3 x \\ x<- list]
//Start = points4 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = points4 [[]] //[]

//addSum :: [[Int]] -> [[Int]]
addSum list = list ++ [sum list]
addSum1 list = [addSum x\\x<-list]
//Start = addSum1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]] 

//Use foldr to check if the square root of each integer in a list are all integers. */

//f9::[Int] ->Bool

//f9 myList = foldr (\ x y = y && ((sqrt (toReal x))==(toReal(toInt (sqrt (toReal x)))))) True myList
//f8 list = foldr (\x y = y && (sqrt(toReal x)) == (toReal(toInt(sqrt(toReal x))))) True list
//Start = f8 [4,16,9] //True

//Start = f8 [1,8] //False

SquareChecker n 
|sqrt (toReal n) == toReal(toInt(sqrt(toReal n))) = True
=False
//Start = SquareChecker 8



check (a,b,c) 
|(c*c) == (a*a) + (b*b) = True && a>0 && b>0 &&c>0

|(c*c) <> (a*a) + (b*b) = False && a>0 && b>0 &&c>0
f1 list = removeDup[x \\ x<-list | check x]

//Start = f1 [(3,4,5),(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f1 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]


f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 list = f8aux (filter checkValid list)

f8aux :: [(Int,Int,Int)]->[(Int,Int,Int)]
f8aux [] = []
f8aux [a:b] = [a] ++ f8 (filter (notDup a) b)

checkValid :: (Int,Int,Int) -> Bool
checkValid (x,y,z) = (a<>b)&&(b<>c)&&(a+b>c)&&(a^2 + b^2 == c^2)
    where
    sorted = sort[x,y,z]
    a = sorted!!0
    b = sorted!!1
    c = sorted!!2

notDup :: (Int,Int,Int) (Int,Int,Int) -> Bool
notDup (x,y,z) (a,b,c) = (sort list1) <> (sort list2)
    where
    gcd1 = gcd x (gcd y z)
    gcd2 = gcd a (gcd b c)
    list1 = map (\elem = elem/gcd1) [x,y,z]
    list2 = map (\elem = elem/gcd2) [a,b,c]

//Start = max 3 2

minifinder [a:b] = [foldr (\x y = min x y) a b]


mini list = [minifinder x\\x<-list]

//Start = mini [[42,420],[24,240]]




