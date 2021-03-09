module Quizz
import StdEnv
/* 
fun :: ({Int}, {Int}) -> [Int]
fun (a,b) = [x\\x<-uni|not(isMember x int)] ++ [x\\x<-int|not(isMember x uni)]
 where
  uni = [x\\x<-:a]
  int = [x\\x<-:b]
/*
//Start = fun ({1,2,3,4},{3,4,5,6})

isleaf Leaf = True
isleaf _ = False
getNode (Node x l r) = x
getprimechild (Node x l r)
|isleaf l || isleaf r = []
|isprime(getNode l) || isprime (getNode r) = [x] ++ (getprimechild l)++ (getprimechild r) 
=(primechild l) ++ (primechild r)*/
:: Q = {nom :: Int, den :: Int}

simplify :: Q -> Q
simplify {nom = a, den = b}
| b == 0 = abort "Division by Zero Error"
| b < 0 = simplify {nom = -1 * a, den = -1 * b}
= {nom = a/g, den = b/g}
where
    g = gcd a b

//Start = simplify {nom=36, den= -4}

instance / Q
where
    / x y = simplify{nom = x.nom * y.den, den = x.den * y.nom}

fracDivision :: Q Q -> Q
fracDivision a b = a/b

//Start = fracDivision {nom=5, den=1} {nom=6, den=5} //{nom=25, den=6}
//Start = fracDivision {nom=10, den=2} {nom=3, den=4} //{nom=20, den=3}
//Start = fracDivision {nom=0, den=2} {nom=100, den=4} //{nom=0, den=1}
//Start = fracDivision {nom=15, den=2} {nom=3, den=12} //{nom=30, den=1}

//We have to write a function that return the nodes of tree
/*
half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)
*/
//instance + Q
//where
//+ a b = simplify{nom = a.nom*b.den + den = a

:: Color = Red | Yellow | Green | Blue | Purple | Violet
:: ColorCombo = { color1 :: Color, color2 :: Color}

instance == Color
where
    == Red Red = True
    == Yellow Yellow = True
    == Green Green = True
    == Blue Blue = True
    == Purple Purple = True
    == Violet Violet = True
    == _ _ = False

colorList :: [Color]
colorList = [Red,Yellow,Green,Blue,Purple,Violet]

//6.
//Write a function that when given a color, returns its complement.
//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green
colorComp :: Color -> Color
colorComp clr  = colorList!!index
where
 clrind = hd[y\\x<-colorList & y <- [1..(length colorList)] | x==clr]
 index = (clrind + 2) rem (length colorList)

/*
This solution utilizes a technique called mapping
in which I used a list to map the colors to integers,
which are conveniently provided by the list's indices.
*/


//Start = colorComp Red //Blue
//Start = colorComp Blue //Red
//Start = colorComp Green //Violet
//Start = colorComp Purple //Yellow



//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.
colorCombo :: Color -> [ColorCombo]
colorCombo clr = [{color1 = clr, color2 = x}\\x<-colorList | x<>clr]

//Start = colorCombo Red //[{color1=Red, color2=Yellow},{color1=Red, color2=Green},{color1=Red, color2=Blue},{color1=Red, color2=Purple},{color1=Red, color2=Violet}]
//Start = colorCombo Blue //[{color1=Blue, color2=Red},{color1=Blue, color2=Yellow},{color1=Blue, color2=Green},{color1=Blue, color2=Purple},{color1=Blue, color2=Violet}]


//we are are making fun that check that the Num is amicable

perfect n = (sqrt (toReal n)) == toReal(toInt(sqrt (toReal n)))
fun1 n = [(x,y,toInt(sqrt(toReal(x^2+y^2))))\\x<-[1..n],y<-[1..n]|perfect (x^2+y^2) && toInt(sqrt(toReal(x^2+y^2)))<= n]

//Start = fun1 10

:: Point = {x::Real,y::Real}
:: Triangle = {a :: Point, b:: Point, c :: Point}
sign :: Point Point Point -> Real
sign p1 p2 p3 = (p1.x-p3.x)*(p2.y-p3.y)-(p2.x-p3.x)*(p1.y-p3.y)
isInside :: Point Triangle -> Bool
isInside p tri
|(d1<0.0 || d2<0.0 || d3 < 0.0)&& (d1>0.0 || d2>0.0 || d3 > 0.0) = False
=True
where
 d1 = sign p tri.a tri.b
 d2 = sign p tri.b tri.c
 d3 = sign p tri.c tri.a

//Start = isInside {x = 0.0, y = 0.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // True
//Start = isInside {x = 3.0, y = 4.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // False
//Start = isInside {x = 0.0, y = 0.0} {a = {x = 0.0, y = 0.0}, b = {x = 0.0, y = 0.0}, c = {x = 0.0, y = 0.0}} // True
//Start = isInside {x = 0.0, y = 1.0} {a = {x = -1.0, y = 1.0}, b = {x = 1.0, y = 1.0}, c = {x = 1.0, y = 1.0}} // True
/*
isPrime n = isEmpty[x\\x<-[2..n-1] |n rem x == 0]
prime n = [x\\x<-[1..n]|isPrime x]
//Start = prime 100
*/

sieve :: [Int] -> [Int]
sieve [p:xs] = [p: sieve [ i \\ i <- xs | i rem p <> 0] ]
//Start = takeWhile ((>)n) (sieve [2..] )
isPrime n
|n<=1 = False
= isEmpty[x\\x<-[2..(n-1)]  | n rem x == 0]
prime2 :: Int Int ->(Int, Int, Int)
prime2 n x
|x==n = (0,0,0)
|isPrime x && isPrime (n-x) = (n,x,n-x)
= prime2 n (x+1)
primes :: Int ->[(Int,Int,Int)]
primes n = [(prime2 x 2)\\x<-[4,6..n] ]
check :: Int -> Bool
check n = length (filter ((<>)(0,0,0)) (primes n)) == length (primes n)
// primes = takeWhile ((>)n) (sieve [2..] )



biglist m n
|length m > length n = m
|length n > length m = n
instance * [a] | * a
where
 * x y
 |length x == length y = [a*b\\a<-x & b<-y]
 = list ++ drop(length list) (biglist x y)
 where
  list = [a*b\\a<-x & b<-y]

//Start=[1,2]*[3,4,5,6,0]//[3,8,5,6,0]

/*
Given an array find the maximum value and return new array which has all occurrences of
the maximum value removed.
For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, maximum is 5,
so answer should be {1,4,3,3,2,4,1,3,4}.
*/
/*
remove :: Int [Int]->[Int]
remove a aa = filter ((<>)a) aa
remMax :: {Int} -> {Int}
remMax arry = {x\\x<-list}
where
l = [x\\x<-:arry]
maxi = last(sort l)
list = remove maxi l
//Start = remMax {1,4,5,3,3,2,4,5,1,3,4} // {1,4,3,3,2,4,1,3,4}
//Start = remMax {1,42,42,52,452,4} // {1,42,42,52,4}
//Start = remMax {5} // {}
//Start = remMax {} // {}
*/
remax::{Int} -> {Int}
remax arr = {x\\x<-ll}
where
 l  = [x\\x<-:arr]
 maxi = last(sort l)
 ll = filter ((<>)maxi) l

//Start = remax {1,4,5,3,3,2,4,5,1,3,4}


arrFold op ini arr = foldr op ini (list)
//where
 //list = [x\\x<-:arr]

//Start = arrFold (+) 0 {1,2,3,4,5} // 15


//Start = foldr (+) 0 [1,2,3,4,5]

*/


/*
This is the standard definition of Binary Tree.
The Tree can be of a given type 'a'.
At each Node, it will contain a key value of type 'a',
and two subtrees, which are Trees of the same type 'a'.
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
//:: RoseTree a = Node a [(RoseTree a)] | Leaf
//Below are some convenient trees to work with for
//exercises and testing.
ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
myTree Leaf = []
myTree (Node x l r) = (myTree l) ++ [x] ++ (myTree r)
myTree1 t = sum(myTree t)
//Start = myTree1  messyTree


//Start = ourTree

messyTree :: (Tree Int)
messyTree = Node 5(Node 12(Node 8 Leaf (Node 1 Leaf Leaf))(Node 6 (Node 9 Leaf Leaf) Leaf))(Node 2 (Node 3 Leaf (Node 13(Node 100 Leaf Leaf)(Node 21 Leaf Leaf)))(Node 40 (Node 60 (Node 70 (ourTree) Leaf) Leaf) Leaf))
//Start = messyTree

emptyTree :: (Tree Int)
emptyTree = Leaf

singleTree :: (Tree Int)
singleTree = Node 5 Leaf Leaf


//dateTree :: (Tree Date)
//dateTree = Node {year = 2000, day = 4, month=3} (Node {year=1999,month=5,day=1} Leaf Leaf) Leaf //(Node (Date 2000 4 3) (Node (Date 1999 1 5) Leaf Leaf) Leaf)

specialTree :: (Tree Int)
specialTree = Node 30 (Node 15 (Node 7 Leaf Leaf)(Node 22 (Node 17 Leaf Leaf)(Node 27 Leaf Leaf)))(Node 60 (Node 45 Leaf Leaf)(Node 75 Leaf Leaf))

//Start = dateTree

//Functions for a Binary Search Tree

//Getting the value at the node
getNode :: (Tree a) -> a
getNode (Node x l r) = x
//Start = getNode ourTree


//Going down left/right subtree
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
//Start = goL ourTree

//Checking if we're at a leaf
isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False
//Start = isLeaf emptyTree

/*
isLeaf x
| x == Leaf = True
| x <> Leaf = False
*/


//Get a list of subtrees from a node.
getSubTrees :: (Tree a) -> [(Tree a)]
getSubTrees Leaf = []
getSubTrees (Node x l r) = (getSubTrees l) ++ [l] ++ [r] ++ (getSubTrees r)
//Start = getSubTrees ourTree

//Get the min value of a BST
minTree :: (Tree a) -> a
// minTree (Node x l r) 
// | isLeaf l = x
// = minTree l

minTree (Node x Leaf _) = x
minTree (Node _ l _) = minTree l
//Start = minTree messyTree
// minTree t
// | isLeaf (goL t) = getNode t 
// = minTree (goL t)
//Start = minTree ourTree

//Reverse a tree
reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = (Node x (reverseTree r) (reverseTree l))
// reverseTree t 
// | isLeaf t = Leaf
// = (Node (getNode t) (reverseTree(goR t)) (reverseTree(goL t)))
//Start = reverseTree ourTree

//Get the max value of a BST
maxTree :: (Tree a) -> a
maxTree t = minTree(reverseTree t)
//Start = maxTree ourTree


//Extract sublists countaining a specific element

//Start = extractSubLists 3 ourTree

//Get a list of children of a node
getChildren n Leaf = []
getChildren n (Node x Leaf Leaf) = []
getChildren n (Node x l r)
| x == n = [getNode l, getNode r]
| x < n = getChildren n l 
| x > n = getChildren n r
//Start = getChildren 15 specialTree 

//Get the parent of a node

//Start = findParent 13 ourTree
//Start = findParent 15 ourTree
//Start = findParent 19 ourTree

//Check if a Binary Tree is actually a BST

//Start = checkBST ourTree
//Start = checkBST messyTree

//Add a new node to a BST
//addNode :: (Tree a) a -> (Tree a) | < a
//addNode Leaf x = (Node x Leaf Leaf)
//addNode t x
//| x < (getNode t) = (Node (getNode t) (addNode (goL t) x) (goR t))
//| x > (getNode t) = (Node (getNode t) (goL t) (addNode (goR t) x))
//= t

//Start = addNode  singleTree 17
getchild a Leaf = Leaf
getchild a (Node x l r)
|a == x = (Node x l r)
|a < x = getchild a l
=getchild a r

//Start = getchild 1 ourTree
//This fun will add New node in a tree
addNode n Leaf = Leaf
addNode n (Node x l r)
|n==x = (Node n l r)
|n<x = addNode n l
= addNode n r
Start = addNode 16 ourTree



































