module HW4
import StdEnv

// For every sublist, eliminates its elements
// Until the current element is a prime number
// After that, multiply each number by 5
// And remove all elements that end with 0. I.e. divisible by 10.

//Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[15],[35,25,15],[15,25,35,45],[],[]]
//Start = f1 [[1], [4], [2]] // [[],[],[]]
//Start = f1 [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[25,35,45],[35,45],[],[25]]
//prime n 
//| n <= 1 = True
//prime n = not(isEmpty[x \\ x<-[2..(n-1)] |  n rem x == 0])
 
//prime1 list = [x*5 \\x<- dropWhile prime list | x rem 2 <>0]
//primelist list  = [prime1 x\\ x<- list]
//Start = primelist [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[15],[35,25,15],[15,25,35,45],[],[]]
//Start = primelist [[1], [4], [2]] // [[],[],[]]
//Start = primelist [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5,4]] // [[25,35,45],[35,45],[],[25]]

// Write function that returns length of a list
// You must use foldr or foldl

//f2 :: [Int] -> Int
//f2 list = foldr (+) 0 [x/x\\x<-list] 

//Start = f2 [] // 0
//Start = f2 [1,2,3] // 3
//Start = f2 [1] // 1
//Start = length (take 100 [1..]) // 100


// Define function "reverse" using foldr
//f3 :: [Int] -> [Int]

//f3 list = foldr (++) [] [[x]\\x<-list]  
f3 list = foldr (\a b -> b ++ [a]) [] list
///Start = f3 [1,2,3,4,5,6]

//Start = f3 [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = f3 [] // []
//Start = f3 [1] // [1]
prime n = isEmpty[x \\ x<-[2..(n-1)] |  n rem x == 0]
primel n = [x \\ x<-[2..n]| prime x && n rem x ==0]


 

//Start =  factor 420 (sieve 420)
//Start = sieve 42 // [(2, 1), (3, 1), (7, 1)]
//Start = factorize 420 // [(2, 2), (3, 1), (5, 1), (7, 1)]
//Start = primel 1024 // [(2, 10)]
//Start = factorize 1 // []
ListGCD :: [Int] -> Int
ListGCD [a,b:c] = foldr gcd (gcd a b) c


//Start = ListGCD [2, 3, 5, 7] // 1
//Start = ListGCD [2, 4, 6, 8] // 2
//Start = ListGCD [0, 2, 4] // 2
//Start = ListGCD [-6, -15] // 3



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

repeet n list
|n==1 = list
= list ++ repeet (n-1) list
ListGenerator :: Int -> [Int]
ListGenerator n = flatten[repeet b [a,b,0]\\a<-[1..n],b<-[1..n]]
//Start= ListGenerator 1 //[1,1,0]
//Start= ListGenerator 2 //[1,1,0,1,2,0,1,2,0,2,1,0,2,2,0,2,2,0]
//Start= ListGenerator 3 //[1,1,0,1,2,0,1,2,0,1,3,0,1,3,0,1,3,0,2,1,0,2,2,0,2,2,0,2,3,0,2,3,0,2,3,0,3,1,0,3,2,0,3,2,0,3,3,0,3,3,0,3,3,0]
//Start= ListGenerator 4



MultiplicationTable :: Int -> [Int]
MultiplicationTable n = flatten[[x*1,x*2..x*n]\\x<-[1..n]]

//Start=MultiplicationTable 0//[]
//Start=MultiplicationTable 2//[1,2,2,4]
//Start=MultiplicationTable 7//[1,2,3,4,5,6,2,4,6,8,10,12,3,6,9,12,15,18,4,8,12,16,20,24,5,10,15,20,25,30,6,12,18,24,30,36]
//Start=MultiplicationTable -5//[]


minilist [first:rest] = foldr (\x y = min x y) first rest

getmin list = [minilist x\\x<-list]

//Start = getmin [[42, 420], [24, 240]]


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

divisors :: Int Int ->[Int]
divisors a b = [x\\x<-[2..a] | a rem x ==0 && b rem x == 0]
//Start = divisors 6 12 //[2,3,6]
//Start = divisors 7 12 //[]
//Start = divisors 9 15 //[3]
//Start = divisors 128 64 //[2,4,8,16,32,64]



cubeRootTest :: Int -> Bool
cubeRootTest x = not(isEmpty[n\\n<-[1..x]|n*n*n == x])

cubeRoot :: Int -> Int
cubeRoot x = hd[n\\n<-[1..x]|n*n*n == x]

power2 n = isMember n (takeWhile((>=)n)[2^x\\x<-[0..]])

cubes2::[Int]->([Int],[Int])
cubes2 list = ([cubeRoot x\\x<-list|cubeRootTest x], [ toInt((ln (toReal x))/(ln 2.0)) \\x<-list | power2 x])



//Start = cubes2 [64, 16, 24, 15, 1 , 8] //([4,1,2],[6,4,0,3])
//Start = cubes2 [1..10] //([1,2],[0,1,2,3])
//Start = cubes2 [25..60] //([3],[5])


isPrime 1 = False
isPrime n  = isEmpty[x\\x<-[2..(n-1)] | n rem x == 0]
OnlyPrimePosition list = [(list!!(b-1),b)\\b<-[1..(length list)] | isPrime b]

//Start=OnlyPrimePosition []//[]
//Start=OnlyPrimePosition [1,5,8]//[(5,2),(8,3)]
//Start=OnlyPrimePosition [1..19]//[(2,2),(3,3),(5,5),(7,7),(11,11),(13,13),(17,17),(19,19)]
//Start=OnlyPrimePosition [1,-5,4,3,6,-5,-7,9,-10]//[(-5,2),(4,3),(6,5),(-7,7)]



T :: Int -> Int
T 0 = 0
T 1 = 0
T 2 = 1
T x 
|x>2 = Taux 0 0 1 x
= 0

Taux a b c n 
|n>3 = Taux b c (a+b+c) (n-1)
=c

//Start = T 0// 0
//Start = T 10 // 44
//Start = T 20 // 19513
//Start = T 50 // 1697490356184
//Start = T 100 // 4130554068881925393

arm a = sum[y^l\\y<-list] == a
 where
 list = n2l a
 l = length list
armstrong n = take n [x\\x<-[1..] | arm x]

//Start = armstrong 9 // [1,2,3,4,5,6,7,8,9]
//Start = armstrong 15 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208]
//Start = armstrong 20 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834]
//Start = armstrong 21 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725]

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

dropp alist n = [y\\y<-alist & z<-[1..(length alist)] | z<>n]  
fl ::[[Int]] Int -> [[Int]]
fl list n = [dropp x n\\x<-list]

//Start = dropp [1,2,3,4,5] 5
//Start = fl [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] 3  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f3 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]] 3  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f3 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]] 3  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]


fx list = and[gcd x y == 1\\x<-list,y<-list | x <> y]

//Start = fx [] //True
//Start = fx [1,3,7,9] //False (3,9) not
//Start = fx [11,12,13] //True
//Start = lcm 12 36

// Use list compherension
euler n = length[x\\x<-[1..(n-1)] | gcd x n == 1]

//Start = euler 102219 // 4
//Start = euler 100 // 40
//Start = euler 2500 // 1000
//Start = euler 1181 // 1180
//Start = euler 1021904 // 443904



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




//Start = f10 1 //False

//Start = f10 (~235) //False

//Start = f10 2147483647 //True

//Start = f10 0 //False


notfib::Int -> Int
notfib n = fibAux n 1 1

fibAux::Int Int Int -> Int
fibAux i a b | i > 0 = fibAux (i-1) b (2*a+b)
| i < 0 = 0 = a

//Start= notfib 0



table n = flatten[[x*1,x*2..x*n] \\x<-[1..n]]
//Start = table 6




//Start = minii [[2,4],[42,23]]
minlist [] = []
minlist [a:b] = [foldr (\x y = min x y) a b]

//Start = minii [2,3,4,5,6]

leap  y 
|y rem 4 <> 0 = False
|y rem 100 <> 0 = True
|y rem 400 <> 0 = False
=False

check n year = take n [x\\x<-[(year+1)..]| leap x]
//Start = check 4 2000


perfect no = sum[x\\x<-[1..no]| no rem x ==0] == no
//Start = perfect -1
comodiv a b = [x\\x<-[2..a]| a rem x ==0 && b rem x ==0]
//Start = comodiv 6 12


cubeRootTest1 :: Int -> Bool
cubeRootTest1 x = not(isEmpty[n\\n<-[1..x]|n*n*n == x])

//cubeRoot1 :: Int -> Int
cubeRoot1 x
|x<=0 = 0
= hd[n\\n<-[1..x]|n*n*n == x]
//Start = cubeRoot1 216


isPrimelist list = and[isPrime x\\x<-list]
f2 [] = []
f2 [a:b]
|isPrimelist a = [a : f2 b]
= f2 b
fo [] = 0
fo [a:b] = foldl (-) a b

//Start = fo [] //0



//isFactorial n =
//leastfactorial :: Int -> Int
leastfactorial n = hd[prod [1..x]\\x<-[1..n] | prod [1..x] > n]

//Start = leastfactorial 17 // 24
checkl list = and[isEven(x+y)\\x<-list & y<-[1..]]

//Start = checkl [2..10]






