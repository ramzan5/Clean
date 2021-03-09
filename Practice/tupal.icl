module tupal
import StdEnv

//Start = [(x,y,z)\\x<-[1..5] , y<-[1..5], z<-[1..5]| isEven x && isOdd y && isEven z]
/*
Tuples are a way to put things together easily.
*/

list1 = [1124,523532,6757]
list2 = ["James","Jack","John"]
//nolist = ["james",1124]
//pair
james :: (String, Int)
james = ("James",1124)
fun list1 list2 = hd(tl[(x,y)\\x<-list2&y<-list1])
//Start = fun list1 list2

//Start = fst james //"James"
//Start = snd james //1124

//fst and snd ONLY work on tuples of 2 elements

//Start = thd3("James", 1124, True)

//Start = fourth("James",1124,True,'a')

//Start = ("James") //singleton tuples do not exist

//func1 :: Int Int -> (Int,Int)
//func1 x y = (x,y)
//[ something , other ]
//Start = func1 2 3 //(2,3)
//Start = [( a , b )\\ a<-[1..7], b<-[a..7]]

//Start = [( a , b )\\ a<-[1..7], b<-[a..7]  | a+b == 7 ] //[(1,6),(2,5),(3,4)]
func2 :: Int -> [(Int,Int,Int,Bool)]
func2 n =  [  ( a , b , b-a, isEven a && isEven b)  \\ a<-[1..n], b<-[a..n] | a*b == n ]

//Start = func2 1000
/*
//dictionary/map/key pairings
Bank :: [(String,Int)]
Bank = [("John",467),("Jim",0),("Jack",900000001),("Evan",-384859493)]

//lookUp :: String [(String,Int)] -> Int
//lookUp name accountsList = snd(hd[  account \\ account<-accountsList | fst account == name ])
//lookUp name accountsList = hd[ accountBalance \\  ( accountName, accountBalance) <- accountsList | accountName == name]
lookName :: Int [(String,Int)] -> String
lookName n list =  fst (hd[(x,y)\\(x,y)<-list|y == n])

//Start = lookName 0 Bank
//Start = lookUp "Evan" Bank

updateAccount :: [ ( String, Int) ] -> [ ( String, Int, Bool) ]
//updateAccount accountsList = [ ( fst account, snd account, snd account > 0 ) \\  account<-accountsList ]
updateAccount l = [(a,b,b>0)\\(a,b)<-l]

//Start = updateAccount Bank //[("John",467,True),("Jim",0,False),("Jack",900000001,True),("Evan",-384859493,False)]
*/
stimulus :: [ ( String, Int, Bool ) ] -> [ ( String, Int, Bool ) ]
//stimulus accountsList = map (\account | thd3 account == False = (fst3 account, (snd3 account)+1000, True ) = account  ) accountsList
stimulus accountsList = map (\( a, b, c) | c == False = (a, b+1000, True ) = (a,b,c)  ) accountsList

//Start = stimulus (updateAccount Bank)

/*
ask a server for a status on an ip address 10.0.0.1
[10,0,0,1] :: [Int]
(10,0,0,1) :: (Int,Int,Int,Int)
(a,b,c,d)
((10,0,0,1) ,  (True,"node 1",["john","james","jack"]) )
*/


// 7. Check if a list contains 2 equal elements one after the other
// (it can be anywhere in the list)
// for [1,2,3,3,3,2,4,5] is True for [1 .. 5] is False

function :: [Int] -> Bool
function [x] = False
function [] = False
function [x,y:xs]
| x == y = True 
= function [y:xs]  

 

//Start = function [1,2,3,3,5] 
// x = 1, y = 2
//False

//  [  \\  a<-[1..3] & b<-[1..3] ]
// a=1 b=1
// a=2 b=2
// a=3 b=3

// [ \\  a<-[1..3] , b<-[1..3] ]
// a=1 b=1
// a=1 b=2
// a=1 b=3
// a=2 b=1
// a=2 b=2
// a=2 b=3
// a=3 b=1
// a=3 b=2
// a=3 b=3


// Lists
// [1,23,4,52,4]!!2
// [Int], [String], [Bool]
// [5, "Hello"]


// ["James", "Bob"]
// [18, 19]

// ["James", 18]

// (Int, String)

// Start = ("Bob", 19, "prrxnh")




// fst
//Start = fst ("Bob", 19)

// snd
// Start = snd ("Bob", 19)


// fst3
//Start = thd3 ("Bob", 19, 12)


// fst3
// Start = snd3 ("Bob", 19, 12)



// thd3
// Start = thd3 ("Bob", 19, 12)


// splitAt

//Start = splitAt 10 ['functional programming']


// zip

//Start = zip ([1,2,3], [4,5,6])









// Given a list of tuple, return a single tuple which is
// the sum of all tuple (x,y) in which x,y have the same parity (odd, odd)/(even, even)
// minus
// the sum of all tuple (x,y) in which x,y dont have the same parity (odd, even)/(even, odd)

// Fancier way to ask:
// Set of vector in 2D, find the final vector which is the result of
// Oh, Nvm...

//funl list = [x+y\\(x,y)<-list|isEven x && isEven y]
//Start = funl [(2,4),(1,3),(2,10)]
add :: (Int, Int) (Int, Int) -> (Int, Int)
// add a b = (fst a + fst b,snd a + snd b)
add (a, b) (c, d) = (a+c, b+d)

/*
samePar :: (Int, Int) -> Bool
samePar (a, b) = isEven (a+b) // (isEven a && isEven b) || (isOdd a && isOdd b)

tupleSum :: [(Int,Int)] -> (Int, Int)
tupleSum list = (x1-x2,y1-y2) // (-7, -8)
where 
    (x1,y1) = foldr add (0,0) (filter samePar list) // (6,8)
    (x2,y2) = foldr add (0,0) (filter (not o samePar) list) // (13,16)
*/
// Start = tupleSum [] // (0,0)
//Start = tupleSum [(1, 2)] // (-1, -2)
//Start = tupleSum [(1, 2), (4, 4), (8,9), (2,4), (4,5)] // (3, 2)
// (filter samePar list) -> [(4,4), (2,4)]
// (0,0) + (2,4) -> (2,4)
// (2,4) + (4,4) -> (6,8)


//(filter (not o samePar) list) -> [(1,2), (8,9), (4,5)]
// (0,0) + (4,5) = (4,5)
// (4,5) + (8,9) = (12,14)
// (12,14) + (1,2) = (13,16)
//dd (a,b) (c,d) = (a-c,b-d)
//Start = foldr  add (0,0) [(1, 2), (4, 4), (8,9), (2,4), (4,5)]



/*
give a integer and produce a tupple.
calculate its divisors into a list as first element of the tuple,
and mean value of this list as the second element of the tuple.
*/
// mean [1,2,3] = (1+2+3) / 3
mean :: [Int] -> Real
mean list = toReal (sum list) / toReal (length list)

fl3 :: Int -> ([Int], Real)
fl3 n = (list_of_divisors, mean list_of_divisors)
where 
    list_of_divisors = [i \\ i <- [1..n] | n rem i == 0]


//Start = f3 8 //([1,2,4,8],3.75)
// Start = f3 9 //([1,3,9],4.33333333333333)
// Start = f3 15 //([1,3,5,15],6)
// Start = f3 5 // ([1,5], 3)







/*
Given a list of Tuples of Integer, give a list of integers produced if we raise
the first integer to the power of the second integer and keep only the even numbers.
Example :
[(2,4),(3,2)] --->[16]
because (2,4) ->2^4 = 16 and is even
(3,2) -> 3^2=9 is odd
*/
power [] = []
power [(a,b):c] = filter isEven ([a^b] ++ power c)
//Start = power [(2,4),(3,2)]
// power :: [(Int,Int)] -> [Int]
//Start = power [(2,4),(3,2)]//[16]
//Start = power [(1,100),(2,3),(4,5)]//[8,1024]
//Start = power [(3,5),(7,9)]//[]

// map using foldr

// map inc [1,2,3,4] -> [2,3,4,5]
// foldr add (0,0) (filter samePar list)

myMap :: (Int -> Int) [Int] -> [Int]
myMap function list = foldr (\ x y = [function x] ++ y) [] list

// Start = myMap inc [1,2,3]/

// filter using foldr
//myFilter :: (Int -> Bool) [Int] -> [Int]
//myFilter function list = foldr (myFunction function) [] list

myFunction :: (Int -> Bool) Int [Int] -> [Int]
myFunction function x y
| function x = [x] ++ y
= y


//Start = myFilter isEven [1,2,3,4,5]

// filter isEven [1,2,3,4,5,6,7]
 

myFilter :: (Int -> Bool) [Int] -> [Int]
myFilter function [] = []
myFilter function [x:xs]
| function x = [x] ++ (myFilter function xs)
= myFilter function xs

// why tuples!?
// [("hossam", 123), ("A", 1234)]
// fst, snd, fst3, snd3, thd3

// Start = thd3 ("hossam", 123, 12.5)

func1 :: (a, b, c) -> c
func1 (t, r, w) = w 

//Start = func1 ("hossam", 123, 13.5)

// splitAt

//Start = splitAt 2 [1, 2, 3]
//Start = splitAt 3 ["PROGRAMMING","is","my","Pssion"]
// zip

// Start = zip ([1, 2, 3], [4, 5])


// Make a function which gives the sum of two tuples 
// i.e ourAdd (1, 2) (3, 4) // (4, 6) 

// Given a list of tuples. return a single tuple which is
// the sum of all tuple (x, y) in which x, y have the same parity (odd, odd) (even, even)
// minus
// the sum of all tuple (x, y) in which x, y don't have the same parity (even, odd) (odd, even)

ourAdd (t, y) (x, z) = (t+x, y+z)

ourMin (t, y) (x, z) = (t-x, y-z)

samePar :: (Int, Int) -> Bool
samePar (a, b) = isEven (a+b)

tupleSum :: [(Int, Int)] -> (Int, Int)
tupleSum list = ourMin (sameParTuple) (diffParTuple)
    where
        sameParTuple = foldr (ourAdd) (0, 0) (filter samePar list)
        diffParTuple = foldr (ourAdd) (0, 0) (filter (not o samePar) list)

//Start = ourAdd (1, 2.2) (3, 4.5)

//Start = foldr (ourAdd) (0, 0) (filter samePar [(1,2), (4,4), (3,3)] )
//Start = (filter samePar [(1,2), (4,4), (3,3),(5,3)] )
// Start = tupleSum [] //(0,0)
// Start = tupleSum [(1,2)] //(-1,-2)

// Start = tupleSum [(1,2), (4,4)] //(3,2)


// Given an integer, produce a tuple which contains a list of its divisors and their mean value
// sum list / length list
// 1+2+4+8 / 4 = 3.75
f3 :: Int -> ([Int], Real)
f3 n = (divisors,  getMeanValue (divisors))
    where
        divisors = getDivOfList n

getDivOfList :: Int -> [Int]
getDivOfList n = [x \\ x <- [1..n] | n rem x == 0]

getMeanValue :: [Int] -> Real
getMeanValue list = toReal (sum list) / toReal (length list)

// Start = getDivOfList 5
tuplegen n = (list,Meanz)
 where
 list = [x\\x<-[1..n]|n rem x == 0]

 Meanz = toReal(sum(list))/ toReal (length list)
//Start = tuplegen 12
// Start = f3 8 //([1, 2, 4, 8], 3.75)
// Start = f3 9 //([1, 3, 9], 4.33333333333333)
// Start = f3 15 //([1, 3, 5, 15], 6)


/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */


Router :: [(a->b)] [(Int,a)] -> [b]
Router listFunc listRoute
| isEmpty listFunc || isEmpty listRoute = []
= [ (listFunc!!(x-1)) y\\ (x, y) <-listRoute]

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

//Start = Router [isEven,isOdd] [] //[]


 //Write a function that reverses tuples from a list if the tuple members sum up to an even number.

//f4 :: [(Int, Int)] -> [(Int, Int)]
f4 [] = []
f4 list = [(x,y)\\(x,y)<-list|isOdd (x+y)] ++ [(y,x)\\(x,y)<-list|isEven (x+y)]
//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]

// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]
f9 (list1,list2,list3) = [(x,y,z)\\x<-list1 & y<-list2 & z<-list3|x*y==z]
//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]

/*
Tuple is a way to contain and transport/carry different types of data.
Lists can only contain data of the same type. ["John","James","Jim"] [28494,284859,2948]
Lists can NOT contain different types. ["John",5748]
("James",8485) :: (String,Int)
*/

//Dictionary. Maps
Bank :: [(String,Int)]
Bank = [ ("James",1234), ("John",42), ("Jimmy",0), ("Evan", -654738476738234738), ("John",9001) ] 

getAccountBalance :: String -> Int
getAccountBalance name = snd(hd[ individualAccount \\ individualAccount<-Bank | fst individualAccount == name])

//Start = getAccountBalance "John"

updateAccount :: (String,Int) -> (String, Int, Bool)
//updateAccount account = ( fst account, snd account , (snd account) > 500)
updateAccount ( _ , 0 ) = ( "broke bastard", 100, False )
updateAccount ( name , balance )
| balance < 0 = ( "broke bastard", 0, False )
= ( name , balance, balance > 500)
//("James",123) -> ("James", 123, True)
//Start = updateAccount ("James",123) //("James",123,False)
//Start = updateAccount (Bank!!2)

eligiblePeople :: [(String,Int)] -> [String]
eligiblePeople ourBank = result
where
    updatedAccounts = map updateAccount ourBank
    filteredAccounts = filter (\ (_,_, status) = status  ) updatedAccounts
    accountNames = map (\ (name,_,_) = name)  filteredAccounts
    result = accountNames

//Start = eligiblePeople Bank

eligiblePeople2 :: [(String,Int)] -> [String]
eligiblePeople2 ourBank =  [ name \\ (name, _, status) <- (map updateAccount ourBank) |  status ]

//Start = eligiblePeople2 Bank

//( something , other , another )
//(  a,  b , c)

// func :: ipadress -> (status, (status report)) 
//         10.0.0.1     Offline,  (10.0.0.1  ,  "node 1")
//         (10,0,0,1)  (False,  ( (10,0,0,1) ,  "node 1"   )   )
//         (Int, Int, Int, Int) (Bool,  ( (Int,Int,Int,Int) ,  String   )   )










