module HW2
import StdEnv

// Write a function that takes an Int 'n' and
// generates a list of fibonacci numbers,
// which are less than or equal to 'n'. 
// https://en.wikipedia.org/wiki/Fibonacci_number

//fibo :: Int -> [Int]

fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fiblist 1 = [0,1,1]
fiblist n = fiblist(n-1) ++ [fib n]
fibo n = takeWhile ((>=)n) (fiblist n) 
//Start = fibo 15
//fib 1 = [0,1,1]
//fib n = fib(n-1) ++ fib(n-2)
//Start = fib 
 

//Start = fibo 0 
// [0]
// Start = fibo -1 // []
// Start = fibo 15 // [0,1,1,2,3,5,8,13]
// Start = fibo 1 // [0,1,1]
// Start = fibo 55 // [0,1,1,2,3,5,8,13,21,34,55]


// Write a function that takes Int 'n' and
// checks if 'n' is a prime number or not.
// Please handle the case of negative numbers.
// Note: 0 and 1 are not prime numbers.

prime :: Int -> Bool

prime n
| n<=1 = False
= mainfun n (n-1)
mainfun :: Int Int -> Bool
mainfun x y
|y == 1 = True
|x rem y == 0 = False
= mainfun x (y - 1)




//Start = prime 0 // False
//Start = prime 1 // False
//Start = prime 28736 // False


// Write a function that takes Int 'n' and
// checks if 'n' is a palindrome or not.
// A palindrome is a number which reads
// identicaly both forwards and backwards.


Numtolist :: Int -> [Int]
Numtolist n

| n >=0 && n < 10= [n]
= Numtolist (n/10) ++ [n rem 10] 

isPalindrome n
| Numtolist n == reverse (Numtolist n) = True
= False

//Start = isPalindrome 0 // True
//Start = isPalindrome 55 // True
//Start = isPalindrome 49594 // True
//Start = isPalindrome 1337 // False
//Start = isPalindrome -57975 // False


//Start = insert 3[2,4,6,8]

notfib::Int -> Int
notfib n = fibAux n 1 1

fibAux::Int Int Int -> Int
fibAux i a b | i > 0 = fibAux (i-1) b (2*a+b)
| i < 0 = 0 = a

//Start= notfib 3

//Start = insert 5 [1,2,3,4,6]

//Start = insert 5 [2,4 .. 10] // [2,4,5,6,8,10]


//Start = merge [1..6] [1..10]

//Start = merge [2,5,7] [1,5,6,8]
//Start = repeatn 5 8
//Start = iterate ((*)2) 10

sieve :: [Int] -> [Int] 
sieve [p:xs] = [p: sieve [ i \\ i <- xs | i rem p <> 0]] 
//Start = take 100 (sieve [2..])
count n list = length(filter((==)n) list)

//Start = count 3 [2, 3, 4, 2, 2, 4, 2, 1] // 4

//Start = removeAt 2 [2,3,4,6,7]

//Start = removeMember 3 [2,3,4,5,3]

//Start = map ((+)5) [1,2,3] // [6,7,8] 


//Start = until ((<)20) ((+)2) 2

glist = take 10[x*5\\x <-[1..]| isOdd x]
//Start = glist

pro x = prod x
prolist list = map pro list

//Start = prolist [[1..5],[1..12],[1..44]]

clist list = unzip list
//Start = clist [(1, 2), (3,1), (8, 4), (5, 7), (8, 9)]


insertl list n = insertAt 1 n list
insertl1 list n = [insertl x n\\ x<-list]

//Start = insertl1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 5





//Start = zip[[1,2,3], [4,5], [6,7,8], []] // [(1,6),(2,9),(3,21),(4,0)]


dlist :: [Int] -> Bool
dlist [] = False
dlist [x] = False
dlist [x,y:xs]
| x==y = True
= dlist [y:xs]


dlist1 x =  foldr (||) False (map check (zip (x, tl x)))

check :: (Int,Int) -> Bool
check (x,y) = (x==y)

//Start = dlist [1,2,3,4,3,2,4,8,5,5]

//build n = take 10[[x..y] \\ x<-[1..n],y<-[1..n] ]

//build n = [[1..x]\\x<-[1..n]]
//Start = build 10


* 1.
In 1D dimension, a segment is represented by a tuple (a, b) as its endpoints. (covers values from a to b, a < b)
Given a list of segments, check whether they form a continuous interval or not.
An interval (left, right) is continuous <=> every value from left to right is covered by at least one segment

Output: a boolean indicates the outcome

Explanation for sample:
A unit segment (segment of length 1) will be visualize as -

[(1, 2), (3, 4)] we have 2 segments (1,2) and (3,4), visualize
(1,2):-
(3,4):  -
You can see that the values between 2 and 3 are not covered by any segment

[(2, 4), (1, 3), (2, 4)] we have 3 segments (2, 4), (1,3) and (2,4), visualize
(2,4): --
(1,3):--
(2,4): --
You can see that the values between 2 and 3 are covered by all segments
*/

Contin [] = [False]
Contin[a] = [True]
Contin list
|snd a >= fst b || a == b = [True : Contin [b:c]]
= [False : Contin [b:c]]
where
[a,b:c] = sort list
Continuous alist = and (Contin alist)

//Start = Continuous [(1, 2), (3, 4)] // False
//Start = Continuous [(2, 4), (1, 3), (2, 4)] // True

/* 2.
Given a natural number as the input, perform prime factorization on that number.

Output: [(prime factor, its power)], sorted by the factor value in ascending order.

Explanation for sample:
42   = 2^1 * 3^1 * 7^1
420  = 2^2 * 3^1 * 5^1 * 7^1
1024 = 2^10
1 has no prime factor -> empty
*/

sieve n = takeWhile ((>)n) [2:[x\\x<-[3..] | x rem 2 <>0]]

factor _ [] = []
factor n [a:b]
|n rem a == 0 = [a] ++ factor (n/a) [a:b]
= factor n b

factorize n = [(x,length (filter ((==)x) (factor n (sieve n))))\\x<-(sieve n) | length (filter ((==)x) (factor n (sieve n))) >0]

Start =  factor 420 (sieve 420)
//Start = sieve 42 // [(2, 1), (3, 1), (7, 1)]
//Start = factorize 420 // [(2, 2), (3, 1), (5, 1), (7, 1)]
//Start = factorize 1024 // [(2, 10)]
//Start = factorize 1 // []

/* 3.
Given a list of tuple: [(Real -> Real, [Real])]
(
  i.e the first value of a tuple is a function Real->Real (~, sqrt, etc)
      the second value is a list of Real
)

Output: For every tuple, return the maximum value of the sublist (2nd) after applying the function (1st)

Explanation for sample:
[(~, [1.0, 2.0, 3.0]), ((+) 1.0, [0.0, 2.0, 3.2])]

for (~, [1.0, 2.0, 3.0]):
  list after applying ~ to every element: [-1.0, -2.0, -3.0] -> max = -1.0
for ((+) 1.0, [0.0, 2.0, 3.2])):
  list after applying + to every element: [1.0, 3.0, 4.2] -> max = 4.2
=> final output: [-1.0, 4.2]
*/
maxi list = last (sort list)
FunctionMax :: [(Real -> Real, [Real])] -> [Real]
FunctionMax list = [maxi[a x\\x<-b] \\(a,b) <-list]
//Start = FunctionMax [(~, [1.0, 2.0, 3.0]), ((+) 1.0, [0.0, 2.0, 3.2])] // [-1, 4.2]

/* 4.
Find the GCD of a list using foldr
Hints: For any 3 integers a, b, c: Can GCD([a, b, c]) ever be greater than GCD([a, b])?
*/

ListGCD :: [Int] -> Int
ListGCD [a,b:c] = foldr gcd (gcd a b) c
//Start = ListGCD [2, 3, 5, 7] // 1
//Start = ListGCD [2, 4, 6, 8] // 2
//Start = ListGCD [0, 2, 4] // 2
//Start = ListGCD [-6, -15] // 3

/* 5.
Write a factorial function using foldr and
apply it using map to a list of numbers as input,
returning a list of factorials in the end.
*/

fact n = foldr (*) 1 [1..n]
mapfact :: [Int] -> [Int]
mapfact list = map (fact) list
//Start = mapfact [] // []
//Start = mapfact [3,4,6,5] // [6,24,720,120]
//Start = mapfact [1..10] // [1,2,6,24,120,720,5040,40320,362880,3628800]

/* 6.
Write a function which takes a list of numbers
returns the numbers which are palindromes or powers of 10.
A palindrome is a number or a word which is identical when read backward and forward.

For example: 123 is NOT a palindrome. 12321 is a palindrome.
1, 10,100,1000,10000 are powers of 10, but 1100 is not
*/

n2l a
|a<10 = [a]
= n2l(a/10) ++ [a rem 10]

isPal n = (n2l n) == reverse (n2l n)


pow10 n =  isMember n (takeWhile ((>=)n) [10^y\\y<-[0..]])

PalinOrPow :: [Int] -> [Int]
PalinOrPow list = [x\\x<-list | isPal x || pow10 x]

//Start = PalinOrPow [1,212,43,55,727,123,100] // [1,212,55,727,100]
//Start = PalinOrPow [1,44,90,1001,100000,76,89,1223,998]//[1,44,1001,100000]
//Start = PalinOrPow []//[]
//Start = PalinOrPow [33]//[33]

/* 7.
Find the least common multiple of two integers
(The least common multiple (LCM) of two integers is
the smallest integer which is a multiple of both of them)
tip: The LCM of two numbers can be found more easily by first finding
their greatest common divisor (GCD)

Explanation for sample:
LCM 12 36:
  36 rem 12 = 0
  36 rem 36 = 0
  and no integer in [1..35] has this property.
*/

LCM :: Int Int ->Int
LCM a b = hd[x\\x<-[(max a b)..] | x rem a==0 && x rem b == 0]
//Start = LCM 12 36 //36
//Start = LCM 96 32 //96
//Start = LCM 35 225 //1575
//Start = LCM 76 36 //684

/* 8.
Given a list of tuples denote the points on a cartesian plane
(
  e.g the list [(4.0, 2.0), (0.0, 0.0)] represents 2 points
    the first point has x = 4, y = 2
    the second point has x = 0, y = 0 (the origin)
)
Check if they are collinear (i.e if they all lie on one straight line)

Output: a boolean indicates the outcome

Explanation for sample:

[(1.0,0.0),(2.0,2.7),(2.0,2.7),(2.0,2.7)] -> True
  There are only 2 non-duplicate points (1.0, 0.0) and (2.0, 2.7), so they obviously lie on the same line

[(1.0,1.0),(2.0,2.0),(3.0,3.0),(5.0,5.0)] -> True
  Since x = y for all points, they all belong to line with equation y = x

(1.7,4.4),(1.1,3.2),(4.1,9.2),(2.5,6.0),(4.1,~9.2)] -> False
  If you draw this sample, you will need more than 1 straight line to connect all of them.
*/
Collinear ::[(Real,Real)] -> Bool
Collinear list
|(length[filter ((==) a) list\\a<-list| length(filter ((==) a) list) == 1]) == 1 = True
|and[x==y\\(x,y)<-list] = True
=False
//Start = Collinear [(1.0,0.0),(2.0,2.7),(2.0,2.7),(2.0,2.7)] //True
//Start = Collinear [(1.0,1.0),(2.0,2.0),(3.0,3.0),(5.0,5.0)] //True
//Start = Collinear [(1.5,1.5),(2.5,2.5),(3.5,3.5),(5.0,5.0)] //True
//Start = Collinear [(1.7,4.4),(1.1,3.2),(4.1,9.2),(2.5,6.0),(4.1,~9.2)] //False
//Start = Collinear [(1.5,1.5),(2.5,2.7),(3.5,3.5),(5.0,5.0)] //False
//Start = Collinear [(1.7,-4.4),(1.1,3.2),(4.1,9.2),(1.7,4.4)] //False

/* 9.
Given a number, you have to generate a list with the following pattern:
ListGenerator 2 -> [1,1,0,1,2,0,1,2,0,2,1,0,2,2,0,2,2,0]
Hint:
  You should find the pattern (every third element is 0, how many time a triplet are repeated, etc)
  Take a look at the last example for better understanding

Explanation for sample:
  Since the main idea is to find the pattern, explaining the sample makes this too easy :(
*/
repeet n list
|n==1 = list
= list ++ repeet (n-1) list
ListGenerator :: Int -> [Int]
ListGenerator n = flatten[repeet b [a,b,0]\\a<-[1..n],b<-[1..n]]
//Start= ListGenerator 1 //[1,1,0]
//Start= ListGenerator 2 //[1,1,0,1,2,0,1,2,0,2,1,0,2,2,0,2,2,0]
//Start= ListGenerator 3 //[1,1,0,1,2,0,1,2,0,1,3,0,1,3,0,1,3,0,2,1,0,2,2,0,2,2,0,2,3,0,2,3,0,2,3,0,3,1,0,3,2,0,3,2,0,3,3,0,3,3,0,3,3,0]
//Start= ListGenerator 4
/*
[1,1,0,1,2,0,1,2,0,1,3,0,1,3,0,1,3,0,1,4,0,1,4,0,1,4,0,1,4,0,
 2,1,0,2,2,0,2,2,0,2,3,0,2,3,0,2,3,0,2,4,0,2,4,0,2,4,0,2,4,0,
 3,1,0,3,2,0,3,2,0,3,3,0,3,3,0,3,3,0,3,4,0,3,4,0,3,4,0,3,4,0,
 4,1,0,4,2,0,4,2,0,4,3,0,4,3,0,4,3,0,4,4,0,4,4,0,4,4,0,4,4,0]
*/

/* 10.
Write a function which takes an integer and creates a multiplication table of that number stored in a list
Multiplication table: In a 2D matrix, the value at row = i, col = j is i * j

Explanation for sample:
Multiplication table for 6:
[
  1, 2, 3, 4, 5, 6
  2, 4, 6, 8, 10, 12
  3, 6, 8, 12, 15, 18
  4, 8, 12, 16, 20, 24
  5, 10, 15, 20, 25, 30
  6, 12, 18, 24, 30, 36
]
*/

MultiplicationTable :: Int -> [Int]
MultiplicationTable n = flatten[[x*1,x*2..x*n]\\x<-[1..n]]

//Start=MultiplicationTable 0//[]
//Start=MultiplicationTable 2//[1,2,2,4]
//Start=MultiplicationTable 6//[1,2,3,4,5,6,2,4,6,8,10,12,3,6,9,12,15,18,4,8,12,16,20,24,5,10,15,20,25,30,6,12,18,24,30,36]
//Start=MultiplicationTable -5//[]



1.
Given a list of sublists of Int
Write a function which returns a list containing
the minimum of every sublist
You should use foldr when finding the minimum.
Ignore empty sublists.
*/
minlist :: [Int] -> [Int]
minlist [] = []
minlist [a:b] = [foldr (\x y = min x y) a b]
GetMin :: [[Int]] -> [Int]
GetMin list = flatten[(minlist l)\\l<-list]
//GetMin [a:[b]]
//|isEmpty a = []
//= [(foldr (\x y = min x y) (hd a) a)]/// ++ GetMin [b]  
//Start = min [2,4,2,6,0,4]

//Start = GetMin [[42, 420], [24, 240]] // [42, 24]
///Start = GetMin [[], [1], [2,3], [4,5,6]] // [1, 2, 4]
//Start = GetMin [[], [1], [2, ~3], [4, ~5, 6]] // [1, -3, -5]
//Start = GetMin [[]] // []


/*
2.
Given a list of tuples, return a single tuple which is
the sum of all tuple (x,y) in which x,y have the same parity (odd, odd)/(even, even)
minus the sum of all tuple (x,y) in which x,y dont have the same parity (odd, even)/(even, odd)
For example:
TupleSum [(1, 2), (4, 4)]
(4,4) has the same parity. We take that and subtract (1,2), which has opposite parity.
*/
//(1,1) (2,2) (1,3) = (1+2-1,1+2-3)
/**
TupleSum :: [(Int, Int)] -> (Int, Int)
TupleSum list = (fst same-fst diff,snd same - snd diff)
where
same = (sum[fst (x,y)\\(x,y)<-list | and[isEven x,isEven y] || and[isOdd x,isOdd y]],sum[snd (x,y)\\(x,y)<-list | and[isEven x,isEven y] || and[isOdd x,isOdd y]])
diff = (sum[fst (x,y)\\(x,y)<-list | (isEven x && isOdd y) || (isOdd x && isEven y)],sum[snd (x,y)\\(x,y)<-list | (isEven x && isOdd y) || (isOdd x && isEven y)])
*/
TupleSum :: [(Int, Int)] -> (Int, Int)
TupleSum list = same - diff
where
same = sum[x\\x<-list |  isOdd (fst x) == isOdd (snd x)]
diff = sum[x\\x<-list |  isOdd (fst x) <> isOdd (snd x)]

instance + (Int,Int)
where
+ (a,b) (c,d) = (a+c,b+d)
instance - (Int,Int)
where
- (a,b) (c,d) = (a-c,b-d)
instance zero (Int,Int)
where
zero = (0,0)
//Start = (1,2) + (3,4)
//Start = sum[(1,2),(3,4)]
//Start = TupleSum [] // (0,0)
//Start = TupleSum [(1, 2)] // (-1, -2)
//Start = TupleSum [(1, 2), (4, 4)] // (3, 2)
//Start = TupleSum [(2, 6),(3, 4),(1, 2),(5, 9),(3, 6)] //(0,3)


/*
3.
Write a function that takes a list of tuples,
each tuple consisting of a predicate function and
a list of Int.
Return a list containing the sum of the sublists
where all elements return True for the predicate.
For example:
Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])]
[1..10] does not return True to isEven for all elements.
[1,3,5,7] returns True for isOdd for all elements. We sum it to 16, add to list.
[5..10] returns True for greater than 3 for all elements. We sum to 45, add to list.
*/
//(cond, [Int])
conditionalFun :: [((Int->Bool),[Int])] -> [Int]
conditionalFun alist = [sum list\\(fun,list)<-alist | and[fun x\\x<-list]]
//Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])] //[16,45]
//Start = conditionalFun [((\x = True),[4,7..35]),((\x = False),[1..])] //[209]
//Start = conditionalFun [(isEven,[]),(isOdd,[])] //[0,0]
//Start = conditionalFun [] //[]


/*
4.
Write a function which takes a list of numbers and returns a list
containing only the palindromes.
A palindrome is a number or word which is identical when
read backwards and forwards.
For example: 123 is NOT a palindrome. 12321 is a palindrome.
*/



n2l :: Int -> [Int]
n2l n
|n<10 = [n]
= n2l (n/10) ++ [n rem 10]
isPalin a = (n2l a) == reverse(n2l a)
//Start = intList 458512512
//Start = isPal 1234
//Start = isPal 12321

Palindromes::[Int]->[Int]
Palindromes list = [x\\x<-list | isPalin x]
//Start = Palindromes [1,212,43,55,727,123,100] // [1,212,55,727]
//Start = Palindromes [76,89,1223,998]//[]
//Start = Palindromes []//[]
//Start = Palindromes [33]//[33]


/*
5.
Write a function which generates a list of the first n leap years starting from
a year x. If either of the arguments is negative output an empty list.
A leap year is divisible by 4 but is NOT divisible by 100 UNLESS it is divisible by 400
From Wikipedia:
if (year is not divisible by 4) then (it is a common year)
else if (year is not divisible by 100) then (it is a leap year)
else if (year is not divisible by 400) then (it is a common year)
else (it is a leap year)
*/
isLeap x
|((x rem 4 ==0) && (x rem 100 <> 0)) || (x rem 400 == 0) =True
= False
LeapYears :: Int Int ->[Int]
LeapYears year no
|year<0 || no <0 = []
= take no [x\\x<-[year+1..] | isLeap x]

//Start=LeapYears 1999 4 // [2000,2004,2008,2012]
//Start = LeapYears 1804 7 //[1808,1812,1816,1820,1824,1828,1832]
//Start = LeapYears -2000 4 //[]
//Start = LeapYears 2000 -9//[]


/*
6.
Write a function that takes a number and determine whether it is a perfect number or not!
A perfect number is a natural number that equals the sum of all its proper divisors.
A proper divisor is every divisor of a number excluding the number itself.
Example : isPerfect 6 // True
(Because the proper divisors of 6 are 1, 2 and 3 so their sum is equal
to 6 so it is true)
*/
isPerfect :: Int ->Bool
isPerfect n
|n<=0= False
= n == sum[x\\x<-[1..(n-1)] | n rem x ==0]
//Start = isPerfect 6 //True
//Start = isPerfect 496 //True
//Start = isPerfect 11//False
//Start = isPerfect 1 //False
//Start = isPerfect 0 //False
//Start = isPerfect -1 // False


/*
7.
Given two integers, return a list of all common divisors
of two intergers (excluding 1)
*/
divisors :: Int Int ->[Int]
divisors a b = [x\\x<-[2..a] | a rem x ==0 && b rem x == 0]
//Start = divisors 6 12 //[2,3,6]
//Start = divisors 7 12 //[]
//Start = divisors 9 15 //[3]
//Start = divisors 128 64 //[2,4,8,16,32,64]


/*
8.
Given a list of intergers
find all the cube numbers(n^3) and write (n) to the first list; for example 8 -> 2
(A cube number is a number that is the product of three numbers which are the same)
find all the numbers which are powers of 2 (2^n) and write the exponent n to the
second list; for example 64 -> 6
*/

cubeRootTest :: Int -> Bool
cubeRootTest x = not(isEmpty[n\\n<-[1..x]|n*n*n == x])

cubeRoot :: Int -> Int
cubeRoot x = hd[n\\n<-[1..x]|n*n*n == x]
//Start = cubeRoot 8

powerOf2Test :: Int -> Bool
powerOf2Test x = isMember x list
where
    pow2 = [2^a\\a<-[0..]]
    list = takeWhile ((>=)x) pow2

cubes2::[Int]->([Int],[Int])
cubes2 list = ([cubeRoot x\\x<-list|cubeRootTest x], [ toInt((ln (toReal x))/(ln 2.0)) \\x<-list | powerOf2Test x])
//Start = cubes2 [64, 16, 24, 15, 1 , 8] //([4,1,2],[6,4,0,3])
//Start = cubes2 [1..10] //([1,2],[0,1,2,3])
//Start = cubes2 [25..60] //([3],[5])


/*
9.
Write a function that will take a list of Integers and
will return a list of tuples (a,b) where b is every prime index of the given
list and a is the value of the list at that index.
Ignore 1 as a prime.
*/
isPrime 1 = False
isPrime n  = isEmpty[x\\x<-[2..(n-1)] | n rem x == 0]
OnlyPrimePosition list = [(list!!(b-1),b)\\b<-[1..(length list)] | isPrime b]

//Start=OnlyPrimePosition []//[]
//Start=OnlyPrimePosition [1,5,8]//[(5,2),(8,3)]
//Start=OnlyPrimePosition [1..19]//[(2,2),(3,3),(5,5),(7,7),(11,11),(13,13),(17,17),(19,19)]
//Start=OnlyPrimePosition [1,-5,4,3,6,-5,-7,9,-10]//[(-5,2),(4,3),(6,5),(-7,7)]


/*
10.
Write function to calculate n-th Tribonacci number.
The nth Tribonacci number is defined by the equation:
T(n) = T(n-1) + T(n-2) + T(n-3)
With the starting parameters:
T(0) = 0, T(1) = 0, T(2) = 1
Your solution must be implemented efficiently via
tail recursion.
*/
T :: Int -> Int
T 0 = 0
T 1 = 0
T 2 = 1
T x
| x > 2 = TAux 0 0 1 x
= 0

TAux :: Int Int Int Int -> Int
TAux a b c n
| n > 3 = TAux b c (a+b+c) (n-1)
= c
//Start = T 1// 0
//Start = T 10 // 44
//Start = T 20 // 19513
//Start = T 50 // 1697490356184
//Start = T 100 // 4130554068881925393


/*
11.
An m-digit Armstrong Number is a number which is equal to sum of digit’s m-th powers.
For example - 153 is a 3 digit Armstrong number: 153 = (1*1*1) + (5*5*5) + (3*3*3).
Write a function which finds the first n Armstrong Numbers
*/
arm a = sum[y^l\\y<-list] == a
where
list = n2l a
l = length list
armstrong n = take n [x\\x<-[1..] | arm x]


//Start = armstrong 9 // [1,2,3,4,5,6,7,8,9]
//Start = armstrong 15 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208]
//Start = armstrong 20 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834]
//Start = armstrong 21 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725]


/*
Test Vectors, for your convenience.*/
a = {x0 = 1, x1 = 2, x2 = 1}
b = {x0 = 3, x1 = 2, x2 = 3}
c = {x0 = 1.0, x1 = 2.0, x2 = 3.0}
d = {x0 = 2.5, x1 = 5.0, x2 = 7.5}
e = {x0 = 4.0, x1 = 5.0, x2 = 6.0}
f = {x0 = 5, x1 = 10, x2 = 5}

:: Vector3 a = {x0::a, x1::a, x2::a}
instance + (Vector3 a) | + a
where
+ m n = {x0=m.x0+n.x0,x1=m.x1+n.x1,x2=m.x2+n.x2}
instance - (Vector3 a) | - a
where
- m n = {x0=m.x0-n.x0,x1=m.x1-n.x1,x2=m.x2-n.x2}
instance == (Vector3 a) | Eq a
where
== m n = m.x0==n.x0 && m.x1==n.x1 && m.x2==n.x2
instance zero (Vector3 a) | zero a
where
zero = {x0=zero,x1=zero,x2=zero}
instance < (Vector3 a) | <,toReal a
where
< x y = sqrt(((toReal(x.x0))^2.0 + (toReal x.x1)^2.0 + (toReal x.x2)^2.0)) < sqrt(((toReal y.x0)^2.0 + (toReal y.x1)^2.0 + (toReal y.x2)^2.0))


//Start = a>b
/*
12.
Define the record type Vector3 taking type 'a'
and define its instances for +,-,Eq,Ord,Zero.
Ord should be defined as one Vector3 is smaller than
another Vector3 when their distance from the origin
is smaller.
Distance from origin of a vector (x0,x1,x2) can be
calculated by the square root of (x0^2 + x1^2 + x2^2)
Test Vectors and Operations.
a = <1,2,1>
b = <3,2,3>
Zero = <0,0,0>
a + b = <4,4,4>
a - b = <-2,0,-2>
a == b = False
a == a = True
a < b = True
a > b = False
*/

/*
13.
Using your defined Vector3 record, determine if two Vector3
are linearly dependent.
Two vectors are linearly dependent if multiplying every component
of one vector with a factor will give you the other vector.
For example:
<1, 2, 3> and <2.5, 5, 7.5> are linearly dependent by a factor of
2.5
Test Vectors and Results
<1.0,2.0,3.0> <4.0,5.0,6.0> = False
<1.0,2.0,3.0> <2.5,5.0,7.5> = True
*/

linearDependent :: (Vector3 Real) (Vector3 Real) -> Bool
linearDependent x y = a == b && b == c
where
    a = x.x0 / y.x0
    b = x.x1 / y.x1
    c = x.x2 / y.x2
//Start = linearDependent c d

/* 1. Eliminate consecutive duplicates of list elements.
 If a list contains repeated elements they should be
 replaced with a single copy of the element
 AND a number given by parameter.
 The order of the elements should not be changed.
 */
f1 [] _ = []
f1 [n] _ = [n]
f1 [a,b:c] n
|a == b = [a,n: f1 (dropWhile ((==)a) c) n]
= [a] ++ f1 [b:c] n
//f1 :: [Int] Int-> [Int]
//Start = f1 [1] 8 // [1]
//Start = f1 [] 1// []
//Start = f1 [1,2,2,3,3,5] 0// [1,2,0,3,0,5]
//Start = f1 [1,1,1,4,4,5,6,7,7,7,7] 9// [1,9,4,9,5,6,7,9]
//Start = f1 [1,2,3,4,5,6] 0// [1,2,3,4,5,6]
//Start = f1 [1,1,1,1,1,1] 5// [1,5]
//Start = f1 [2,2,2,2,2,2,2] 666// [2,666]

/* 2. Given a list of sublists of Int,
keep only the lists where all numbers
are prime numbers.
*/

isPrimelist list = and[isPrime x\\x<-list]
f2 [] = []
f2 [a:b]
|isPrimelist a = [a : f2 b]
= f2 b

//f2::[[Int]]->[[Int]]
//Start = f2 [] //[]
//Start = f2 [[],[4,5,6],[7,11],[7..11]] //[[],[7,11]]
//Start = f2 [[1..10],[2,3,7,5],[1,3,5,7],[21]] //[[2,3,7,5]]


// 3.  Delete the n-th element of each sublist in the list.
dropp alist n = [y\\y<-alist & z<-[1..(length alist)] | z<>n]  
f3 ::[[Int]] Int -> [[Int]]
f3 list n = [dropp x n\\x<-list]

//Start = f3 [1,2,3,4,5] 1
//Start = f3 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] 3  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f3 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]] 3  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f3 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]] 3  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]




// Determine the prime factors of a given positive integer.
// Construct a flat list containing the prime factors in ascending order.
//primeFactors :: Int -> [Int]
primeFactors n
|n<=1 = []
= [x\\x<-[1..n] | isPrime x && n rem x == 0]
//Start = primeFactors 0 // []
//Start = primeFactors -5 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 614889782588491410 // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]



// Rotate a list N places to the left.
fl list n = drop n list ++ take n list
//Start = fl [1,2,3] 2   // [3,1,2]
//Start = f1 [] 3 // []
//Start = f1 [6] 5 // [6]
//Start = fl [1,2,3,4,5,6,7,8] 3 // [4,5,6,7,8,1,2,3]



/* 1. Given a list of integers, decide if they are pairwisely relatively prime or not.
(Two integers are relatively prime (or coprime)
if there is no integer greater than one that divides both of them)
*/
//f1::[Int]->Bool
fx list = and[gcd x y == 1\\x<-list,y<-list | x <> y]
//Start = fx [] //True
//Start = fx [1,3,7,9] //False (3,9) not
//Start = fx [11,12,13] //True



// Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as
// the number of positive integers r (1 <= r < m) that are coprime to m.

// Use list compherension
euler n = length[x\\x<-[1..(n-1)] | gcd x n == 1]

//Start = euler 10 // 4
//Start = euler 100 // 40
//Start = euler 2500 // 1000
//Start = euler 1181 // 1180
//Start = euler 1021904 // 443904

// Generate the list of all possible (Day, Month) tuples in a given year.
// Make sure to take care of different number of days in different months.
    // January - 31 days
    // February - 28 days in a common year and 29 days in leap years
    // March - 31 days
    // April - 30 days
    // May - 31 days
    // June - 30 days
    // July - 31 days
    // August - 31 days
    // September - 30 days
    // October - 31 days
    // November - 30 days
    // December - 31 days
// Make sure to take care of leap years.
// leap year:
// if (year is not divisible by 4) then (it is a common year)
// else if (year is not divisible by 100) then (it is a leap year)
// else if (year is not divisible by 400) then (it is a common year)
// else (it is a leap year)



// Start = dayMonth 2016


// You are given record representing set Q (rational numbers)
// Write function simplifyRational that takes rational number and brings it to normal form.
// So 15/20 should be 3/4, 2/4 should be 1/2, ...

:: Q = { num :: Int, denom :: Int }
simplifyRational :: Q -> Q
simplifyRational {num = n, denom = d}
|d == 0 = abort "Invalid"
|d<0 = {num = ~n/g,denom = ~d/g}
= {num = n/g,denom = d/g}
where
g = gcd n d
//Start = simplifyRational { num = 15, denom = 20 } // (Q 3 4)
//Start = simplifyRational { num = 2, denom = 4 } // (Q 1 2)
// Start = simplifyRational { num = 1, denom = 3 } // (Q 1 3)
// Start = simplifyRational { num = 5, denom = 1 } // (Q 5 1)
// Start = simplifyRational { num = 15, denom = -20} // (Q -3 4)



//  It is hypothesized that every even number greater than two can be expressed as the sum of two primes.
//  For example, 4 = 2+2, 6 = 3+3, 8 = 3+5.
//  Check if this is true for all even numbers in the range 4 to n?
//  Hint/Requirement: Use list comprehension

// check :: Int -> Bool
//check n = [x == y+z\\x<-[4..n],y<-[2..n],z<-[2..n] | isPrime y && isPrime z]
//Start = check 1000 // True
// Start = check 100000 // True
// Start = check 500 // True
// Start = check 10 // True


// 1. For every sublist, eliminates its elements
// Until the current element is a prime number
// Requirement:
//  - Use list comrehension to determin the prime number!
//  - Use map instead of recursion
//  - Use dropWhile
f11 :: [[Int]]->[[Int]]
f11 list = [dropWhile isnotPrime x\\x<-list]
where
isnotPrime x = not(isPrime x)
//Start = f11 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[2,3,4],[7,6,5,4,3,0],[3,5,7,9],[],[]]
//Start = f11 [[1], [4], [2]] // [[],[],[2]]
//Start = f11 [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[5,6,7,8,9,10],[7,8,9],[],[5,4]]

//2. A positive number in the form like: 10, 200, 8, 1000, 40, 1, 9, 7000, 30000000
// (which has only one non-zero digit at first place) is called a "clean number"(0 is not included)
// find all clean numbers in the list of lists and write to a list
isClean n
|n<= 0 = False
|n<10 = True
|a>0 && and[y==0\\y<-b] =  True
=False
where
[a:b] = n2l n
f21 :: [[Int]] ->[Int]
f21 [] = []
f21 [a:b] = [x\\x<-a | isClean x] ++ f21 b
//Start = f21 [[1,2,7,10,50,102,33],[],[0,9,90],[11,980,20]] //[1,2,7,10,50,9,90,20]
//Start = f21 [[1..20],[10,20..60],[30,20.. -10]]//[1,2,3,4,5,6,7,8,9,10,20,10,20,30,40,50,60,30,20,10]

//3. find the middle element of each sublists of list.(hint:use !!)
// list of even length like [0,1,2,3] choose 2
// add them together using foldr
// suggest using only one function
f31 list = foldr (+) 0 [x!!((length x)/2)\\x<-list]
//Start =f31 [[1],[1,2],[1,2,3]] //5
//Start =f31 [[1],[1,2],[1,2,3],[3,3,0,8,9]] //5
//Start = f31 [[10,20,30],[1,3]] //23





// Generate list of Fibonacci numbers which are less than given n and are even.
FibAux :: Int Int Int -> Int
FibAux 1 _ b = b
FibAux n a b = FibAux (n-1) b (a+b)

//Alternate Solution:
//The second solution only needs one helper function.
FibList :: Int -> [Int]
FibList n = takeWhile ((>)n) [FibAux x 0 1\\x<-[1..n]]
f12 n = [x\\x<-(FibList n) | x>=0 && isEven x]
//Start = f12 10 // [2,8]
//Start = f12 1000 // [2,8,34,144,610]
//Start = f12 100000 // [2,8,34,144,610,2584,10946,46368]
//Start = f12 1000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296]
//Start = f12 10000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296,1134903170,4807526976]

// Define function myLength, which returns length of a list
// You must use foldr
myLength list = foldr (\x y = y + 1) 0 list
//Start = myLength [] // 0
//Start = myLength [1,2,3] // 3
//Start = myLength (take 100 [1..]) // 100
//Start = myLength [1..] // Heap full

// Define function "reverse" using foldr
myReverse list = foldr (\x y = y ++ [x]) [] list
//Start = myReverse [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = myReverse [] // []
//Start = myReverse [1] // [1]


/**
  * Write a function that takes a list of coefficients for a polynomial
  * and evaluates it at an integer given as the second parameter.
  * A list such as [1,6,9] would represent the the polynomial x^2+6x+9.
  * Note: Exponentiation via (^) or a custom exponential function
  * is NOT allowed.
  *
  * For example: Evaluate [1,6,9] 2 = 25
  * Hint: Use Horner's Method
  * e.g. 3x^2 + 2x -4 = -4 + 2x + 3x^2 = -4 + x(2 + x(3)))
  *
  * Total: 50pts
  */
  //Start = Evaluate [1,6,9] 2 //25
//Start = Evaluate [1337] 12345 //1337
//Start = Evaluate [] 9001 //0
//Start = Evaluate [243,810,1080,720,240,32] (~2) //-1024

/**
  * Write a function that takes two lists of Strings,
  * one containing First Names and the other containing
  * Family Names, and creates a list where each sublist
  * will contain the First Names matched with the
  * Family Names.
  * In the case that the list of Family Names has only
  * one Family name, assign it to every Frist Name.
  */
MakeFamily a b
|length b == 1 = [[x,y]\\x<-a , y<-b]
= [[x,y]\\x<-a & y<-b]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le", "Figueiredo", "Sitt"] //[["Hossameldin","Abdin"],["Zuka","Tsinadze"],["Nicola","Cenic"],["Tringa","Sylaj"],["Ying","Xue"],["Nghia","Le"],["Pedro","Figueiredo"],["Evan","Sitt"]]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Zsok"] //[["Hossameldin","Zsok"],["Zuka","Zsok"],["Nicola","Zsok"],["Tringa","Zsok"],["Ying","Zsok"],["Nghia","Zsok"],["Pedro","Zsok"],["Evan","Zsok"]]
//Start = MakeFamily [] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le Minh", "Figueiredo", "Sitt"] //[]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] [] //[]

// 1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.

//f1 :: Int -> Int
f13 n = (x/10) rem 10
where
x = abs n
//Start = f13 1234 //3

//Start = f13 5 //0

//Start = f13 (~5564) //6


// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.

// Return 0 for an empty list.

f22 :: [Int] -> Int
f22 [] =0
f22 list = foldr (\x y = y-x) (hd list) (tl list)

//Start = f22 [10,1,2,3] //4

//Start = f22 [1,2,3,4] //-8

//Start = f22 [1000,500,250,125] //125

//Start = f22 [] //0


// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

//f3 :: Int -> [Int]
f32 n = [x\\x<-[1..n] | n rem x == 0 && isPrime x]

//Start = f32 36 //[1,2,3]

//Start = f32 524287  //[1,524287]

//Start = f3 0 //[]


// 4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.
rvrt (a,b)
|isEven (a+b) = (b,a)
= (a,b)
//f4 :: [(Int, Int)] -> [(Int, Int)]
f4 list = [rvrt x\\x<-list]

//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]


// 5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

//f5 :: [Int] -> [[Int]]
f5 list = [[x*1,x*2,x*3,x*4,x*5]\\x<-list]

//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]

//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]


// 6. Given an integer n, find the minimal k such that

// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

// In other words, find the smallest factorial which is not less than n.

// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17
//isFactorial n =
//leastfactorial :: Int -> Int
leastfactorial n = hd[prod [1..x]\\x<-[1..n] | prod [1..x] > n]
//Start = isFactorial 17
//Start = leastfactorial 17 // 24

//Start = leastfactorial 1 // 1

//Start = leastfactorial 5 // 6

//Start = leastfactorial 25 // 120


// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

//f7 :: [Int] -> Bool
f7 list = and[isEven (x+y)\\x<-list & y<-[1..]]
//Start = f7 [1..10] //True

//Start = f7 [1,2,3] //True

//Start = f7 [2,3,4] //False

//Start = f7 [1,3,4,5] //False

//Start = f7 [1,2,3,4,6,7] //False

//Start = f7 [] //False


// 8. Write a function that removes consecutive duplicates in a list.

//f8 :: [Int] -> [Int]
f8 [] = []
f8 [a,b:c]
|a==b = f8 (dropWhile ((==)a) c)
=[a] ++ f8 [b:c]  

//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]

//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]

//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5]


// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

//f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]
f9 (a,b,c) = [(x,y,z)\\x<-a & y<-b & z<-c | x*y == z]

//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]


// 10. Write a function that checks if a number is a Mersenne Prime.

// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1

//f10 :: Int -> Bool
f10 n = n == power - 1
where
power = hd[2^x\\x<-[1..] | 2^x > n]

//Start = f10 7 //True

//Start = f10 1 //False

//Start = f10 (~235) //False

//Start = f10 2147483647 //True

//Start = f10 0 //False


/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
 
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */

Router :: [(a->b)] [(Int,a)] -> [b]

Router alist blist
|isEmpty alist || isEmpty blist = []
= [(alist!!(a-1)) b\\(a,b)<-blist]
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
Listing [a,b,c,d] = [a+b,c+d]

//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

//Start = Listing [13,29,1030,307] //[42,1337]

//Start = Listing [] //[]


/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
 
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

//SeqCheck :: [Int] -> Bool

//Start = SeqCheck [1..10] //True

//Start = SeqCheck [1,2,3] //True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False


/**4
  * Write a function that checks if each elements in the list appear even times.
 
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */
check [] = [[]]
check [a:b] = [[a] ++ filter ((==)a) b] ++ check (filter ((<>)a) b)
checkEven :: [Int] -> Bool
checkEven list = and[isEven (length x)\\x<-(check list)]


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
DotProd a b = sum[x*y\\x<-a & y<-b]
//Start = DotProd [4,6,3] [6,3,7] //63

//Start = DotProd [6,3,7] [4,6,3] //63

//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0


//6
// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. */

//TwoLists :: [Char] -> ([Char], [Char])
TwoLists list = ([x\\x<-list | isMember x ['1'..'9']],filter (\char = char >= 'a' && char <= 'z') list)
//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

//Start = TwoLists [] // ([],[])


//7
// Given a list of lists, for each list, extract the first, middle and last element. */

Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [[]] = []
Points3 list = [(x!!0,x!!((length x)/2),last x) \\x<-list]
//Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = Points3 [[]] //[]


//8
//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle.
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */
f82 [] = []
f82 [a:b]
|length (filter ((==)a) b) >= 1 = [a] ++ f82 b
= f82 b
f81::[(Int,Int,Int)]->[(Int,Int,Int)]
f81 list = [(x!!0,x!!1,x!!2)\\x<-(f82 listed)]
where
listed = [sort[x,y,z]\\(x,y,z)<-list]
//Start = f81 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f81 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]


//9
//Use foldr to check if the square root of each integer in a list are all integers. */
f91 list = foldr (\x y = y && (toReal(toInt(sqrt(toReal x))) == sqrt(toReal x))) True list
//f9::[Int] ->Bool

//Start = f9 [] //True

//Start = f91 [4,16,9] //True

//Start = f91 [1,8] //False

 
///10 Insert sum of elements as last element in every sublist of a list. */

addSum :: [[Int]] -> [[Int]]
addSum list = [x ++ [sum x]\\x<-list]

//Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]




 