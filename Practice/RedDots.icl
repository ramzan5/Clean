module RedDots
import StdEnv

/////////////////////CHOCOLATE///////////////////////////////////////////////////////////////

// Given following three values, the task is to find the total number of maximum chocolates you can eat.

//     money : Money you have to buy chocolates
//     price : Price of a chocolate
//     wrap : Number of wrappers to be returned for getting one extra chocolate.

// It may be assumed that all given values are positive integers and greater than 1.

// EXAMPLE :
 
// Input :   money = 16, price = 2, wrap = 2
// Output :   15
// Price of a chocolate is 2. You can buy 8 chocolates from
// amount 16. You can return 8 wrappers back and get 4 more
// chocolates. Then you can return 4 wrappers and get 2 more
// chocolates. Finally you can return 2 wrappers to get 1
// more chocolate.
chocoaux :: Int Int ->Int
chocoaux wrapers return
|wrapers == return = 1
= (wrapers/return) + chocoaux (wrapers/return) return
choco :: Int Int Int -> Int
choco money price wrap = money/price + chocoaux (money/price) wrap

//Start = choco 16 2 2//15
//Start  = choco 10 2 2//8

///////////////////////////COVERBOARD////////////////////////////////

// Given an area of N X M.
// You have infinite number of tiles of size 2^i X 2^i, where i = 0, 1, 2,… so on.
// The task is to find minimum number of tiles required to fill the given area with tiles.

area n m =  prod(takeWhile ((>=)(n*m)) [x^y\\x<-[2], y<-[0,1..]])//[prod[x^y]\\x<-[2], y<-[0,1..] | prod[x^y] == n*m]//[takeWhile ((>=)(n*m)) [2^x]\\x<-[0..] | sum[2^x] == n*m]
//Start = area 10 10

//////////////////////////SUPERDIGIT/////////////////////////////////

// We define super digit of an integer

// using the following rules:

// If x has only digit, then its super digit is x
// Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.


// For example, super digit of 9875
// will be calculated as:
// super_digit (9875) = super_digit (9+8+7+5)
//                    = super_digit (29)
//                    = super_digit (2+9)
//                    = super_digit (11)
//                    = super_digit (1+1)
//                    = super_digit (2)
//                    = 2.

// You are given two numbers n and k. You have to calculate the super digit of P.
// Where P is created when n is concatenated k times. That is, if n = 123 and k = 3, then P = 123123123

n2l :: Int -> [Int]
n2l a
|a<10 = [a]
= n2l (a/10) ++ [a rem 10]

concatenate :: Int Int -> [Int]
concatenate n k
|n == 1 = n2l k
= concatenate (n-1) k ++ (n2l k)

dig :: [Int] -> Int
dig p
|length p == 1 = hd p
= dig (n2l (sum p))

SuperDigit :: Int Int -> Int
SuperDigit n k = dig (concatenate n k)

//Start = SuperDigit 123 3//9
//Start = SuperDigit 9 4//9

//////////////////////TRIANGLE//////////////////////////

// Given an array of integers, print a sum triangle from it such that the first level has all array elements.
// From then, at each level number of elements is one less than the previous level
// and elements at the level is be the Sum of consecutive two elements in the previous level.
triangleAux [] = []
triangleAux [a] = []
triangleAux [a,b] = [a+b]
triangleAux [a,b:c] = [a+b: triangleAux [b:c]]

triangle :: [Int] -> [[Int]]
triangle [] = [[]]
triangle [a] = [[]]
triangle [a,b] = [[a+b]] ++ [[a,b]]
triangle [a,b:c]
= triangle (triangleAux [a,b:c]) ++ [[a,b:c]]

//Start = triangle [1,2,3,4,5]    //[ [48],
                                //  [20, 28]
                                //  [8, 12, 16]
                                //  [3, 5, 7, 9]
                                //  [1, 2, 3, 4, 5] ]

/////////////////////MERGETREES////////////////////////

// Given two binary trees and imagine that when you put one of them to cover the other, 
// some nodes of the two trees are overlapped while the others are not.
// You need to merge them into a new binary tree. The merge rule is that if two nodes overlap, 
// then sum node values up as the new value of the merged node. 
// Otherwise, the NOT null node will be used as the node of new tree.



	// Tree 1                     Tree 2                  
    //       1                         2                             
    //      / \                       / \                            
    //     3   2                     1   3                        
    //    /                           \   \                      
    //   5                             4   7              

    //   Merged tree:
	//      3
	//     / \
	//    4   5
	//   / \   \ 
	//  5   4   7

:: Tree a = Node a (Tree a) (Tree a) | Leaf
Tree1 = Node 1 (Node 3 (Node 5 Leaf Leaf) Leaf)(Node 2 Leaf Leaf)
Tree2 = Node 2 (Node 1 Leaf (Node 4 Leaf Leaf))(Node 3 Leaf (Node 7 Leaf Leaf))
Tree3 = Node 1 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
add :: (Tree a) (Tree a) -> (Tree a) | + a
add Leaf (Node x l r)  = Node x l r
add (Node x l r) Leaf = Node x l r
add Leaf Leaf = Leaf
add (Node x l r) (Node y k s) = Node (x+y) (add l k) (add r s)
//Start = add Tree2 Tree1

////////////////PASSWORD/////////////////////////////////	

// A password is considered strong if below conditions are all met:

//     It has at least 6 characters and at most 20 characters.
//     It must contain at least one lowercase letter, at least one uppercase letter, and at least one digit.
//     It must NOT contain three repeating characters in a row ("...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).

// Write a function strongPasswordChecker(s), that takes a string s as input, and return the MINIMUM change required to make s a strong password. If s is already strong, return 0.

// Insertion, deletion or replace of any one character are all considered as one change.
len word
|length word >= 6 && length word <= 20 = 0
=1
low [] = False
low [a:b] = (isMember a ['a'..'z']) || low b
up [] = False
up [a:b] = (isMember a ['A'..'Z']) || up b
digit [] = False
digit [a:b] =(isMember a ['1'..'9']) || digit b

lud str 
|(low str) && (up str) && (digit str) = 0
=1
rep [] = []
rep [a:b] = [(takeWhile ((==)a) b)]++rep b
		
strongPasswordChecker :: String -> Int
strongPasswordChecker str = len list + lud list
where
	list = [x\\x<-:str]
//Start = strongPasswordChecker "abcdef6R"




