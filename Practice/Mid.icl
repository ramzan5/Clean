module Mid
import StdEnv

/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */

//Router :: [(a->b)] [(Int,a)] -> [b]

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

//Start = Router [isEven,isOdd] [] //[]


/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */
  

//Listing :: [Int] -> [Int]
Listing [] = []
Listing [a] = [a rem 2]
Listing [a,b] = [a..b]
Listing [a,b,c] = [a*(b^c)]
Listing [a,b,c,d] = [a+b, c+d]

//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

//Start = Listing [13,29,1030,307] //[42,1337]


//Start = Listing [] //[]


/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

SeqCheck :: [Int] -> Bool
SeqCheck [] = False
SeqCheck seq = and[isEven (x+y)\\x<-seq & y<-[1..]]

//Start = SeqCheck [1..10] //True

//Start = SeqCheck [1,2,3] //True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False

//Write a function that checks if each elements in the list appear even times.
  
 // For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  

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

DotProd :: [Int] [Int] -> Int
//DotProd [a:b] [x:y] = (a*x) + DotProd b y
DotProd list1 list2 = sum[x*y \\ x<-list1 & y<-list2]
//Start = DotProd [4,6,3] [6,3,7] //63

//Start = DotProd [6,3,7] [4,6,3] //63

//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0




//6 Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. */

//TwoLists :: [Char] -> ([Char], [Char])
TwoLists list = (filter (\char = char >= '0' && char <= '9') list, filter (\char = char >= 'a' && char <= 'z') list)
//TwoLists list = splitAt 3(sort list)
//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

//Start = TwoLists [] // ([],[])



// Given a list of lists, for each list, extract the first, middle and last element. */


Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [[]] = []
//Points3 bigList = [(hd subList, subList!!((length subList)/2),last subList)\\subList<-bigList]
Points3 list = [(hd x, x!!((length x)/2), last x) \\ x<-list]


//Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = Points3 [[]] //[]

//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */

//f8::[(Int,Int,Int)]->[(Int,Int,Int)]

check (a,b,c) 
|(c*c) == (a*a) + (b*b) = True && a>0 && b>0 &&c>0

|(c*c) <> (a*a) + (b*b) = False && a>0 && b>0 &&c>0
f8 list = [x \\ x<-list | check x]
//Start = f8 [(3,4,5)]

//Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]


//Use foldr to check if the square root of each integer in a list are all integers. */

//f9::[Int] ->Bool
f9 myList = foldr (\ x y = y && ((sqrt (toReal x))==(toReal(toInt (sqrt (toReal x)))))) True myList

//Start = f9 [] //True

//Start = f9 [4,16,9] //True

//Start = f9 [1,8] //False


fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fiblist 1 = [0,1,1]
fiblist list =  takeWhile((>=)list)[fib x\\ x<-[0..list]]

//Start = fiblist 100
//ispalindrome list = splitAt list

number n
|n>=0 && n<9 = [n]
= number (n/10) ++ [n rem 10]
isPalin n
|number n == reverse(number n) = True
=False
//Start = isPalin 1234567

lcmfinder [] = 0
lcmfinder [a] = a
lcmfinder [a,b] = lcm a b
lcmfinder [a,b:c] = lcm (lcm a b) (lcmfinder c)
//Start = lcmfinder []
// Start = lcmfinder [1, 10, 400453, 58359, 5389538] // 89966928901863090
// Start = lcmfinder [] // 0
// Start = lcmfinder [5] // 5
// Start = lcmfinder [4,5] // 20
// Start = lcmfinder [1, 2, 3] // 6 

lcmFinder [] = 0
lcmFinder list = foldr (lcm) 1 list


//Start = lcmFinder [1, 2, 3] // 6 
//Start = lcmFinder [1, 10, 400453, 58359, 5389538] // 89966928901863090
//Start = lcmFinder []

f1 :: Int
//f1 =  sum(filter isOdd [x^2\\x<-[1..100]]) 

/*f1 = f1aux 1 
f1aux n
|n^2 < 100 = [n^2: f1aux (n+2)]
= f1aux (n+1)
*/
//| n <> 100 = abort "I can't calculate this: I am a restricted function :( "
f1 = sum(takeWhile ((>)10000)[x^2\\x<-[1..10000]|isOdd x])


//Start = f1 // 166650

Prime n
|n<=1 = False
= isEmpty[x\\x<-[2..(n-1)] | n rem x ==0]
//Start = Prime 1

double x = x + x
quad x = double(double x)
//Start = quad 2.1

fact n
| n == 0 = 1
= n*fact(n-1)
//Start = fact 12
power x n
|n == 0 =1
=x * power x (n-1)
//Start = power 2 5

addTwo a b = toInt(a) + b
//Start = addTwo 2.3 2

alternate n = ~n
//Start = alternate 2
max :: Int Int -> Int
max x y
|x == y = x
|x > y = x
|x < y = y
//Start = max 12 13

triple x = (double x) + x
//Start = triple 9
isodd :: Int -> Bool
isodd x 
| x rem 2 <> 0 = True
= False
//Start = isodd 4
// 5. Check if a number is the sum of two other given numbers.
check1 :: Int Int Int -> Bool
check1 x y z
| x+y == z = True
= False
//Start = check1 1 4 3



add :: Int -> Int
add n = n + 100
//Start = add 12
multi :: Int -> Bool
multi n
| n rem 10 == 0 = True
= False
//Start = multi 100 

//addNum n = sum[1..n]
addNum n 
|n == 1 = 1
= n + addNum (n-1)


//Start = addNum 12
 //Check if an integer is even - in two ways. To divide integer use /, for remainder use rem
//even1 :: Int -> Bool


//Start = even1 34
//Start = even1 45

//version 2.
//even2 :: Int -> Bool


//Start = even2 34
//Start = even2 45



isum::Int->Int
isum n
|n==0 = 0
= isum(n/10) + n rem 10
//Start = isum 12234
take1:: Int [Int] -> [Int]
take1 n [] = []
take1 n [a:b]
|n>0 =[a] ++ take1 (n-1) b
= take (n-1) b
//Start = take1 3 [1,2,3,4,5]

reverse1 :: [Int]->[Int]
reverse1 [] = []
reverse1 [a:b] = reverse1 b ++ [a]
//Start = reverse1 [1,2,3,4,5]
triplesum ::[Int]->[Int]
triplesum [] = []
triplesum [a,b:c] = [a+b] ++ triplesum c
//Start = triplesum [1..10] 
sumlist x
|x == [] = 1
= hd x * sumlist (tl x)
//Start = sumlist [1,2,3,4,5]
length1 [] = 0  
length1 [_ : rest]= 1 + length1 rest
//Start = length1 [1..10]



//Start :: String

monthSrting = ["January","February","March","April","May","June","July","August","September","October","November","December"]

//Start = (monthSrting)!!0


//Given a list of Reals.
//Write a code which will add 1 to every real number from the list which is less than 10
subst2 [] = []
subst2 [a:b]
|a < 10.0 = [a + 1.0] ++ subst2 b
= [a] ++ subst2 b
//Start=subst2 [1.6,12.4,5.4,12.4] //[2.6,12.4,6.4,12.4]

fun x
|x>100 = x
= fun (x+1)
//Start = fun 12

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fac :: Int -> Int
fac n = facAux n 1

facAux :: Int Int -> Int
facAux 0 acc = acc
facAux n acc = facAux (n-1) (acc * n)

//Start = [[4]++[6..10]]

//Start = [ [4] : [[3,2,4],[1..10]]]
doublelist list = [double x \\x<-list|isEven x]
//Start = doublelist [1..10]
//[a,b,c:d]

ev [] = []
ev [a:b] 
| isEven a = [a] ++ ev b
= ev b
//Start = ev [1..10]
lastof :: [a] -> a
lastof [x] = x
lastof [x : y] = lastof y
//Start = lastof [13,26..100]
//keep the head of every sublist e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]

heads [] = []
heads [x:y]
= [hd x] ++ heads y 
//heads [] = []
//heads list = [hd (hd list)] ++ heads (tl list)
//Start = heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
//tails::[[Int]]->[Int]
tails [] = []
tails [x:y] = [x!!((length x)/2)] ++ tails y
//Start = tails [[1,2,3,4],[4,5,7,8,6],[7,8,6,5,9]]

// Write a function that takes an Int 'n' and
// generates a list of fibonacci numbers,
// which are less than or equal to 'n'. 
// https://en.wikipedia.org/wiki/Fibonacci_number

// fibo :: Int -> [Int]


fibo 0 = 0
fibo 1 = 1
fibo n = fib (n-2) + fib(n-1)
fibl 1 = [0,1,1]
fibl n
|n<0 = []
= takeWhile((>=)n)[fibo x \\ x<-[0..n]]

//Start = fibl 1000 // [0]
//Start = fibl -1 // []
//Start = fibl 15 // [0,1,1,2,3,5,8,13]
//Start = fibl 1 // [0,1,1]
//Start = fibl 55 // [0,1,1,2,3,5,8,13,21,34,55]




fib1 :: Int -> (Int, Int) 
fib1 0 = (1,1) 
fib1 1 = (1,1) 
fib1 n = (b,a+b) 
where 
 (a,b) = fib1 (n-1)
//Start = fib1 3

fib2 :: Int Int Int -> Int 
fib2 a b 0 = a 
fib2 a b c = fib2 b (a+b) (c-1)

//Start = fib2 0 1 3

//Write a function which takes a [Int] and returns a
//[Int] containing the middle element of that list.
//Note: lists with odd number of elements will only
//return a list with one middle element, lists with
//even number of elements should return a list with
//two elements.
//middle::[Int]-> [Int]

middle list
| length list == 0 = [-1] 
|isEven (length list) = [(list!!(length(list) / 2))] ++ [(list!!(length(list)/2) - 1)]
= [(list!!(length(list)/2))]
//Start = middle [1..10]//[5,6]
//Start=middle [1,2,3,4,5]//[3]
//Start=middle []//[-1]
//Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.

//f1 :: Int -> Int
f n = abs((n/10) rem 10)


//Start = f 5 //0

//Start = f (~(5564)) //6

// Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.

// Return 0 for an empty list.

//f2 :: [Int] -> Int


f0 [a:b] = foldl (-) a  b





//Start = f0 [10,1,2,3] //4

//Start = fun1 [1,2,3,4] //-8

//Start = fun1 [1000,500,250,125] //125

//Start = fun1 [] //0

// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

//f3 :: Int -> [Int]

prime n = isEmpty[x \\ x<-[2..n-1]| n rem x == 0]

ff n = [x \\ x<-[1..n]|prime x && n rem x ==0]
 

//Start = ff 36 //[1,2,3]

//Start = ff 524287  //[1,524287]

//Start = ff 0 //[]
// Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

//f5 :: [Int] -> [[Int]]


f5 myList = [[x,2*x..5*x]\\x<-myList]


//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]
//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]

twiceof f x = f (f x)
//Start = twiceof inc 2
//Start = (takeWhile((>=)50) (filter isEven[1..100]))
//Start = filter isOdd (map (\x=x^3)[1..100])

//Start = length[1,2,3]

//Start = [-10,0..100]

//keep the head of every sublist e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
//heads :: [[Int]] -> [Int]


heads1 [] = []
heads1 [a:b] = [tl a] ++ heads1 b
//Start = heads1 [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

seqCheckAux :: [Int] -> [Bool]
seqCheckAux [] = [True]
seqCheckAux [x] = [isOdd x]
seqCheckAux [a:b] = [isOdd a] ++ [isEven (hd b)] ++ seqCheckAux (tl b)
seq1 list = and(seqCheckAux list)
//Start = seq1 [1..8]
is n = [n*x \\x<-[1..n]]
//Start = is 100

//Start = [ n \\ n<-[100,99..50]]
listadd list1 list2 = [a + b \\a<-list1 & b<-list2]
//Start = listadd [1,2,3,4,5] [1,2,3,4,5]

//Start = [a \\ a<-[1..3] , b<-[1..a]]

//Start = [ 3^x \\ x<-[1..10] | 3^x <=1000 && 3^x>0]

//Start = [ a + b \\ a<-[1..3] , b<-[1..5] | isEven b && (a+b)>0 ]

genSeq :: Int -> [Int]
genSeq n = [ (-1)^x\\ x<-[1..n]  ]

//Start = genSeq 10

//Start =  [1..5] !! 4
myList = [a+b*c\\a<-[1..3],b<-[1..3],c<-[1..3]]
//Start =  myList !! ((length myList)/2)

extractMid :: [Int] -> Int
extractMid list = list !! ((length list)/2)

//Start = extractMid [1..5]


//Start = [ 1,2,3 : [ n*2 \\ n<-[3..10] | isEven n]   ]
//Start = [ a \\ a<-[1..10] , b<-[1..a]]

myList1 = [ a+b \\ a<-[1..10] , b<-[1..5]]
//Start = myList1 !! ((length myList1)-1)

Fib 0 = 0
Fib 1 = 1
Fib n = Fib(n-1) + fib (n-2)
Fibl 1 = [0,1,1]
Fibl n = takeWhile((>=)n)[Fib x \\x<-[0..n]]
//Start = Fibl 55

Pri n
|n<=1 = False 
=isEmpty[x\\x<-[2..n-1]|n rem x == 0]
//Start = Pri 28736
isPrimeAux :: Int Int -> Bool
isPrimeAux n c
|n == c = True
= n rem c <> 0 && isPrimeAux n (c+1)

isPrime :: Int -> Bool
isPrime n = isPrimeAux n 2
//Start = isPrime 5 // True
//Start = isPrime 0 // False
// Start isPrime 1 // False
// Start isPrime 28736 // False


// 1. Reverse every sublist of a list 

revsub :: [[Int]] ->  [[Int]]
revsub [] = []
revsub [x:y] =   revsub y ++ [reverse x]

//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]]
//Start = [0,5..500]

//Delete the first and the last element of a list.

el list = tl (init list)
//Start = el [1,2,3,4]
// 4. Compute for a given positive n the sum of 2i*(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.
//fll :: Int -> Int

fo 0 = 0
fo n = prod(repeatn n n) + fo (n-1)

//Start = fo 3

// 5. Cut a list in two parts at the middle. E.g. cut [1..10] -> [[1,2,3,4,5],[6,7,8,9,10]]
// and for cut [1..11] the result is [[1,2,3,4,5],[6,7,8,9,10,11]].
//cut :: [Int] -> [[Int]]

//cutlist [] = []
cutlist list = [[list!!x\\x<-[0..len/2-1]],[list!!x\\x<-[len/2..len-1]]]
 where
 len = length list

//Start = cutlist [1..12]

fff n =length [x \\ x <- [2..(n-1)] | n rem x == 0] 

//Start = [-10000,-9500..10000]
// 7. Keep the last elements of the sublists of a list in one list (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]

lastl list = [x!!((length x)/2) \\x<-list]
//Start = lastl [[1,2,3],[5,6],[1],[7,8,9,10]]
ins list = [init x\\x<-list]

//Start = ins [[1,2,3],[5,6],[1],[7,8,9,10]]

poo n = sum(map(\x = x^2) [1..n])

//Start = poo 4


lll lists = [x \\ x <- lists | isEven(length x)] ++ [reverse x \\ x <- lists | isOdd (length x)]


//Start = lll [[1,2,3],[5,6],[1],[7,8,9,10]]
flo a b op = op a b
//Start = flo 2 3 (-) 
//Start = filter ((>=)5) [1..10]
//Start = filter (\x=x<=5) [1..10]
//Start = [x\\x<-[1..10]| x<=5]
//isPrime1 :: Int -> Bool
isPrime1 n = and[n rem x <> 0 \\x<-[2..(n-1)]]
//Start = isPrime1 2
//Start = [ x \\x<-[1..100]|isPrime1 x && x < 50 ]

//incr list = [x+1\\ x <-list]
//Start = incr [1..20]

//Start = map (\x = (x+5) > 7 ) [1..10] //[True,True,True,True,True,True,True,True,True,True]

isInList list1 list2 = filter (\ someNum = isMember someNum list1) list2

//Start = isInList [2,4,7,3,6,4,7,2,1,7] [3,4,5,6,7,8,9] //[3,4,6,7]



//common x y = filter (\a= isMember a x) y
common list list1 = [x+y\\x<-list & y<-list1|isEven x && isEven y]
//Start = common [2,4,7,3,6,4,7] [3,4,5,6,7,8,9]

//Start = foldl (-) 100 [1,2,3]

//Start = foldr (\x y = y ++  [x]) [] [1..10]

foo n = foldl (\a b = a - b) 0 [1..n]
//Start = foo 2
//Start = map foo [1..5]

//Start = filter ((<>)5) [1,3,5,2,3,4,5,5,6]
// 3. Keep every non-empty sublist.
// e.g. [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]] -> [[1, 3, 5],[3,4],[5,7,8,9]]

isEmptyL  :: [list] -> Bool
isEmptyL list = not((length list) == 0)

checkl list = filter (isEmptyL) list
//Start = checkl [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]]
add1 list = [map ((+)1) x\\x<-list]
//Start = add1 [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]] 
//Start = map (\x = x^2) [1..10]

tripleHead :: [Int] -> [Int]
tripleHead [] = []

tripleHead list = [(hd list)*3] ++ (tl list)

triples :: [[Int]] -> [[Int]]
triples listOfList = map (tripleHead) listOfList

//Start = triples [[1..5],[1..10],[],[1],[1,2,3],[1..4]]
//use map to insert n in the middle of every sublist of a list.
//if there is one element in the middle of sublist, insert n before it 
//e.g. if n = 4 , [1,2,3] -> [1,4,2,3] (insert 4 before 2)

middlel n list = [list!!(x)\\x<-[0..len/2-1]] ++ [n] ++ [list!!(x)\\x<-[len/2..len-1]]
 where
 len = length list
addNumLoL n list= map (middlel n) list

//Start = addNumLoL 4 [[1,1,3],[1,2,3,4],[12,33,5,7,8,9]]

my list = foldl (++) [] list
//Start = my [[1,2,3],[1..5],[1..8]]
sqrsProd :: [Int] -> Int
sqrsProd list = foldr (*) (1) (map (\x = x^2) list)

//Start = sqrsProd [1,2,3]
myTask :: [[Int]] -> [Int]
myTask list = map (\ x = foldr (+) 0 x) list
//Start = myTask [[1,2], [1, 3, 4]]

//Start = foldr (\ x list = [x * 2] ++ list) [] [1..10] //[2,4,6,8,10,12,14,16,18,20]
//Start = foldr (\ newElement list | isEven newElement = [newElement^3] ++ list = list ) [] [1..10]


isPrimel n = isEmpty[ x \\ x<-[2..(n-1)] | n rem x == 0]

square n list = [x^2\\x<-list| isPrimel x && x<n]

//Start = square 5 [1..20]
//Start = foldr (\x y = y - x) 10 [1..3]

//Start = foldr (-) 0 [1..3]

factorial1 n = foldr (*) 1 [1..n]
//Start = factorial1 43

//Start = foldr (\ newElement list = [newElement] ++ list ++ [newElement]) [] [1..5]


ulta list = foldr (\x y = [x]++y++[x]) [] list
//Start = ulta [1,2,3,4,98]
//Start = foldr (\ newElement list | isEven newElement = [newElement] ++ list = list) [] [1..10]
//Start = foldr (\x y = x-y) 0 [1, 2]


myLength list = foldr (\x y = y + 1) 0 list
//Start = myLength [1,2,3,4,5]

lisr l = foldr (+) 0 (map(\x = 1) l)

//Start = lisr [1..10]


ff1 [] _ = [] 
ff1 list num
| num > 0 = ff1 ((tl list) ++ [hd list]) (num-1)
= list 

Start = ff1 [1..5] 3










