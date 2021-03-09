module Practice
import StdEnv
//Start = sqrt 4.0

//Start = sum [1..10]
//f a b c = a * b / c
//Start = f 2.0 4.0 6.0

//inc x = x + x^4 
//Start = inc 3
//double x = x + x 
//quadruple x = double (double x) 
//octal x = quadruple (quadruple x)
//Start = octal 2

//factorial n = prod [1 .. n] 
//Start = factorial 12


//signof x 
//| x > 0 = 1 
//| x == 0 = 0 
//| x < 0 = -1 
//Start = signof -8
power x n 
| n == 0 = 1 
| n > 0 = x * power x (n - 1)
//Start = power 2 5  

//Given a list of Reals.
//Write a code which will add 1 to every real number from the list which is less than 10


//subst2 :: [Real] -> [Real]
//subst2 [] = []
//subst2 [a:b]
//|a>10.0 = [a+1.0] ++ (subst2 b)
//= [a] ++ subst2 b
//Start=subst2 [1.6,12.4,5.4,12.4] //[2.6,12.4,6.4,12.4]

//list :: [Real] -> [Real]

//list [] = []
//list [a:b]
//| a < 10.0 = [a] ++ list b
//= [a] ++ list b
//Start = list [1.6,12.4,5.4,12.4] 


//drop x [] = []
//drop x [a:b]

//| x < 1 = [a:b]
//= drop (x-1) b
//Start = drop 3 [1,5,6,7,8,9,56]

//take n [] = []
//take n [a:b]

//| n < 1 = []
//= [a : take(n-1) b]
 
//Start = take 3 [1,3,6,8,10]

//prime n 
//| n>1 && n rem 2 <> 0 = True
//=False
//Start = prime 15

//find a b c = (~b + toInt(sqrt(toReal(b^2 - 4*a*c)))) / 2*a 
//find- a b c = (-b - sqrt(b^2-4a*c)) / 2*a 
//Start = find 1 -2 1

//dis x1 x2 y1 y2 =  toInt(sqrt(toReal((x2-x1)^2 + (y2-y1)^2)))

//Start = dis 2 2 4 6

//even :: Int -> [Int]
//even n = [x\\x<-[0..n]| x<1 && n /2 ]
//Start = even 20 

//prime :: Int -> [int]
//prime n
//|n >= 2 && (n / 2) rem 2 <> 0 = True




breakdown :: Int -> [Int]
breakdown x
| x < 10 = [x]
| otherwise = breakdown (x/10) ++ [x rem 10]
//Start = breakdown 23412324
//prime n
//| n >= 2 && n rem 2  <> 0 && n/2  = True
//=False
//Start = prime 2


// Write a function that takes an Int 'n' and
// generates a list of fibonacci numbers,
// which are less than or equal to 'n'. 
// https://en.wikipedia.org/wiki/Fibonacci_number

//fibo :: Int -> [Int]

fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

//fiblist 1 = [0,1,1]
//fiblist n = fiblist(n-1) ++ [fib n]
//fibo n = takeWhile ((>=)n) (fiblist n) 
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

//isPrime :: Int -> Bool
//isPrime n
//| n > 1 && n rem n <>0 = True
//= False

//Start = isPrime 16
// True
// Start isPrime 0 // False
// Start isPrime 1 // False
// Start isPrime 28736 // False


// Write a function that takes Int 'n' and
// checks if 'n' is a palindrome or not.
// A palindrome is a number which reads
// identicaly both forwards and backwards.


//Numtolist :: Int -> [Int]
/*Numtolist n

| n >=0 && n < 10= [n]
= Numtolist (n/10) ++ [n rem 10] 

isPalindrome n
| Numtolist n == reverse (Numtolist n) = True
= False
*/
//Start = isPalindrome 0 // True
//Start = isPalindrome 55 // True
//Start = isPalindrome 49594 // True
//Start = isPalindrome 1337 // False
//Start = isPalindrome -57975 // False




/*fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
fiblist 1 = [0,1,1]
fiblist n = fiblist(n-1)++ [fib n]
fibo n = takeWhile ((>=)n) (fiblist n)
Start = fibo 55

*/
//odd n =[x*3\\ x<-[1..n]]
//Start = odd 20
//isPrime :: Int -> Bool
/*isPrime n
| n <= 1 =False
= prime n 2
prime a b
|b >= a/2 = False
|a rem b == 0 = False
= True && (prime a (b+1))
*/








//isPrime n = isEmpty[x \\x<-[2..n-1]|x>2 && n rem x == 0] 
//Start = isPrime 2

/*

middle :: [Int] -> [Int]

middle n
//length n == 0 = [-1]
//|isEven (length n) = [n!!((length n / 2) - 1)] ++ [n !! (length n / 2)]
//=[n !! (length n /2)]
//Start=middle [1..10]//[5,6]
//Start=middle [1,2,3,4,5]//[3]
fib2 :: Int Int Int -> 
//Int fib2 a b 0 = a 
fib2 a b c = fib2 b (a+b) (c-1)
//Start = fib2 1 1 5

*/

//odd x = not (isEven x) 
//Start = isOdd 23 // True
//Start = filter (not o isEven) [1..100]

//Start = [x\\x<-[1..100000] | isEven x]
Twice :: (t->t) -> (t->t) 
Twice f = f o f 
//Start = Twice inc 2 // 4
//f = g o h o i o j o k is nicer than f x = g(h(i(j(k x))))
//Lists - Higher order functions Filtering
//takeWhile :: (a?Bool) [a] ? [a] 

//Start = dropWhile isEven [2,4,6,5,7,8,9] 
//Start = map (\x = x*x+2*x*x+1) [1..10] //[4,9,16,25,36,49,64,81,100,121]

plus x y = x + y 
successor :: (Int -> Int) 
successor = plus 1 
//Start = successor 4 // 5

succ = (+) 1 
//Start = succ 5

//Start = map (plus 5) [1,2,3] // [6,7,8]


//Start = snd("world",10)

f::(Int, Char) -> Int
f(n, x) = n + toInt x
//Start = f(10, 'A')
//Start = splitAt 3['Hilovew']
search :: [(a,b)] b -> a | == b 
search [(x,y):ts] s 
| y == s = x  
|otherwise = search ts s



//Start = search [(1,1.4), (2,4.6), (3,9.1)] 9.1 // 9

//double [] = []
//double [a:b] = [a+a: double b]
//Start = double [1, 21, 32, 63] 


//Start = map (\x = x+5) [1,3,5,7,8,9]
//until p f x 
//| p x = x 
//| otherwise = until p f (f x)
//Start = until ((<)10) ((+)1) 0 // 12


//Start = iterate inc 2
Start = foldr (+) 10 [1, 2, 3] 







