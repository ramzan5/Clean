module Tuple1
import StdEnv

//Start = [(x,y,z) \\ x <-[1..5], y<-[1..5], z<-[1..5]|isEven x && isOdd y && isEven z] 
list1 = ["Ali","Rizwan","Izhar"]
list2 = [123,121,124]
f1 list1 list2 = hd(tl[(x,y) \\ x<-list1 & y<-list2])
//Start = f1 list1 list2
//Start = ("Ali")//Single Tuple does not exist
f2 n = [(a,b,isOdd a && isEven b) \\ a<-[1..n]&b<-[1..n]]
//Start = f2 100
Bank :: [(String,Int)]
Bank = [("John",467),("Jim",0),("Jack",900000001),("Evan",-384859493)]
f3 list name = snd(hd[x\\x<-list|fst x == name])
//Start = f3 Bank "Jack" 
f4 list = [(x,y,y>0)\\(x,y)<-list]
//Start = f4 Bank 
f5 list = map (\(a,b,c) | c == True = (a, b+1000, c) = (a,b,c)) list
//Start = f5 (f4 Bank)

// 7. Check if a list contains 2 equal elements one after the other
// (it can be anywhere in the list)
// for [1,2,3,3,3,2,4,5] is True for [1 .. 5] is False
f6 [x] = False
f6 [x,y:xs]
|x == y = True
= f6 xs
//Start = f6 [1,2,5,4,2,2]
//Start = splitAt 12 ['functional programming']

// Given a list of tuple, return a single tuple which is
// the sum of all tuple (x,y) in which x,y have the same parity (odd, odd)/(even, even)
// minus
// the sum of all tuple (x,y) in which x,y dont have the same parity (odd, even)/(even, odd)

// Fancier way to ask:
// Set of vector in 2D, find the final vector which is the result of
// Oh, Nvm...

add (a, b) (c, d) = (a+c, b+d)

samePar (a, b) = isEven (a+b) // (isEven a && isEven b) || (isOdd a && isOdd b)

tupleSum :: [(Int,Int)] -> (Int, Int)
tupleSum list = (x1+x2,y1+y2) // (-7, -8)
where 
    (x1,y1) = foldr add (0,0) (filter samePar list) // (6,8)
    (x2,y2) = foldr add (0,0) (filter (not o samePar) list) // (13,16)

//Start = tupleSum [(2,3),(3,3),(2,4),(1,2)]

//Start = filter samePar [(2,3),(3,3),(2,4),(1,2)]

instance + (a,b)| +a & +b
 where
  (+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  
//Start = (2,3) + (4,5) + (4,4)

/*
give a integer and produce a tupple.
calculate its divisors into a list as first element of the tuple,
and mean value of this list as the second element of the tuple.
*/
// mean [1,2,3] = (1+2+3) / 3

f7 n = (divisor , mean)
 where
  mean = toReal(sum (divisor)) /toReal(length(divisor))
  divisor = [x\\x<-[1..n]|n rem x == 0]





//Start = f7 8 //([1,2,4,8],3.75)
//Start = f7 9 //([1,3,9],4.33333333333333)
//Start = f7 15 //([1,3,5,15],6)
// Start = f3 5 // ([1,5], 3)

/*
Given a list of Tuples of Integer, give a list of integers produced if we raise
the first integer to the power of the second integer and keep only the even numbers.
Example :
[(2,4),(3,2)] --->[16]
because (2,4) ->2^4 = 16 and is even
(3,2) -> 3^2=9 is odd
*/
f8 list = filter isEven([fst x ^ snd x\\x<- list])
//Start = f8 [(1,100),(2,3),(4,5)]
//Start = f8 [(2,4),(3,2)]//[16]
//Start = f8 [(3,5),(7,9)]//[]
//Thid fun will add the tuple of same parity
//Start = foldr (add) (0, 0) (filter (samePar) [(1,2), (4,4), (3,3),(5,3)])

/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */

Router list1 list2
|isEmpty list1 || isEmpty list2 = []
= [(list1!!(x-1)) y \\ (x,y) <- list2]

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

//Start = Router [isEven,isOdd] [] //[]
//Write a function that reverses tuples from a list if the tuple members sum up to an even number.
f9 list = [(a,b)\\(a,b)<- list| isOdd (a+b)] ++ [(b,a)\\(a,b)<- list| isEven (a+b)]


//Start = f9 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f9 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]


/*
Given a list of pairs of name of the person and his/her favourite food.
Make function which returns list of pairs of food and a list of people who likes it.
Note 
*/
favFood [] = []
favFood [(a,b):c] = [(b,[a:[x \\(x,y)<-c|y == b]])] ++ favFood [(n,m)\\(n,m)<-c|b <> m]


//Start = favFood [("Zuka", "apple"),("Ali", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "apple")] // [("apple", ["Zuka", "Ahmed"]),("orange",["Beka"]),("pineapple",["Emad"])]
//Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "pineapple")] // [("apple", ["Zuka"]),("orange",["Beka"]),("pineapple",["Emad","Ahmed"])]


/*
Having a list of tuples, each tuple represent a person in that form (name, age, gender)
Write a function to produce a list of two elements. the older man's name and the older woman's name
i.e : [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] -> ["Hossam", "Nani"]
Note : You can assume that the input for the gender will be "male", "female".
*/

findYounger list = [x\\(x,y,z)<-list|y == maxageMan ] ++ [x\\(x,y,z)<-list|y == maxageFema ]
 where
  maxageMan = hd(reverse(sort ([y\\(x,y,z)<-list|z =="male"])))
  maxageFema = hd(reverse(sort ([y\\(x,y,z)<-list|z =="female"])))



//Start = findYounger [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] // ["Hossam", "Nani"]
//Start = findYounger [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")] // ["Evan", "Tringa"]
// Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]



/*
Decide if a number is triangular number and write the count of levels of triangle. 
Triangular number is a number that can form a triangle.
The output should be in a tuple.
Note : if it is false the count should be -1. 
examples:
1       3         6          10          15
                                          *
                               *          * *
                  *            * *        * * *
        *         * *          * * *      * * * *
*       * *       * * *        * * * *    * * * * *

Note : 0 is not a triangular number
*/
tri n = (n * (n+1))/2
triangular n = (isMember n list, len)
 where
      list = (takeWhile((>=)n)[tri x\\x<-[1..]])
      len = length list
isTri n
| x == False = (False,-1)
=(x,y)
 where
  (x,y) = triangular n



//Start = isTri -1 // (False,-1)
// Start = isTringularNum 0 // (False,-1)
// Start = isTringularNum 1 // (True,1)
//Start = isTringularNum 5 // (False,-1)
//Start = isTringularNum 10 // (True,4)
//Start = isTringularNum 666 // (True,36)

//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (a1, a2) = [x\\x<-l1| not(isMember x l2)] ++ [x\\x<-l2| not(isMember x l1)]
where
    l1 = [x\\x<-:a1]
    l2 = [x\\x<-:a2]

Start = symmetricDiff ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
//Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
//Start = symmetricDiff ({},{}) //[]










