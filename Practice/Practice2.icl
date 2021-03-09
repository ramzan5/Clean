module Practice2
import StdEnv

// Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order. 

//primefactor :: Int -> [Int]




prime 1 = False
prime n = isEmpty[x\\x<-[2..(n-1)] | n rem x == 0]
primefactor n = [x\\x<-[2..n] | prime x && n rem x == 0]

//Start = primefactor -5 // []

//Start = primefactor 1 // []
//Start = primefactor 17 // [17]
//Start = primefactor 24 // [2, 3]




// Write a function that takes a list of numbers and
// breaks it into two lists, which contain even and 
// odd elements from the original list.
// For example: [3,5,6,8,7,9] -> [ [3,5,7,9], [6,8] ]

splitList :: [Int] -> [[Int]]
splitList n = [[x\\x<- n | isOdd x],[x\\x<- n | isEven x]]  

//Start = splitList [56,3,87,5,234,5,0,-4] //[[3,87,5,5],[56,234,0,-4]]
//Start = splitList [1] //[[1],[]]
//Start = splitList [420] //[[],[420]]
//Start = splitList []//[[],[]]



// Write a function that takes a list of integers and gives their least common multiple.


lcmList :: [Int] -> Int

lcmList [] = 0
lcmList [a] = a
lcmList [a,b] = lcm a b
lcmList [a,b:c] = lcm (lcm a b) (lcmList c)

//Start = lcmList [1, 10, 400453, 58359, 5389538] // 89966928901863090
//Start = lcmList [] // 0
//Start = lcmList [5] // 5
//Start = lcmList [4,5] // 20
//Start = lcmList [1, 2, 3] // 6

//Start = snd("12","World")
//f (n, x) = n + toInt x
//Start = f (1, 2)
//Start = splitAt 2  splitAt 2 [1,2,3,45,56,78,8,9]

//search [(a,b):ts] s
//|b ==s= a
//= search ts s
//Start = search [(1,1), (2,4), (3,9)] 3 // 9


zip :: [a] [b] -> [(a,b)] 
zip [] ys = [] 
zip xs [] = [] 
zip [x : xs] [y : ys] = [(x , y) : zip xs ys]
//Start = zip [1,2,3] [1,2,3,4] // [(1,’a’),(2,’b’),(3,’c’)]

//Start = [x * x \\ x <- [1..10] | isEven x] //[4,16,36,64,100]
//Start = [(x,y) \\ x <-[1..6], y <- [1..6]]
//Start = map(\x = x*2*x)[1..10]
qsort [] = []
qsort [a:xs] = qsort [x \\x<-xs| x<a] ++ [a] ++ qsort [x \\ x<- xs|x>=a]
Start = qsort [2,1,5,3,6,9,0,1] 











