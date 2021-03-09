module PN8UOM
import StdEnv

//Write a function that checks if a list of numbers is odd,even,odd,even...
//e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.
checkSeq :: [Int] -> Bool
checkSeq [] = False
checkSeq seq = and[isEven (x+y)\\x<-seq & y<-[1..]]

//Start = checkSeq [1..10] //True
//Start = checkSeq [1,2,3] //True
//Start = checkSeq [2,3,4] //False
//Start = checkSeq [1,3,4,5] //False
//Start = checkSeq [1,2,3,4,6,7] //False
//Start = checkSeq [] //False

 
//Write a function that given a list of arrays, sorts them by
//their greatest element.
//The order of elements in the arrays must be preserved.
//In the case of arrays with equal greatest elements, their
//original order in the list must be preserved.
//For example: sortArrays [{2,3,4},{1,2,3},{3,4}] will
//return [{1,2,3},{2,3,4},{3,4}]





getMax array = last(sort[x\\x<-:array])
sortArrays ::[{Int}]->[{Int}]
sortArrays array =  sortBy (\x y = (getMax x) < (getMax y)) array


    
    
//Start = sortArrays [{4,2,5,6},{1,4,2},{5,2,1,0,3,2}] //[{1,4,2},{5,2,1,0,3,2},{4,2,5,6}]
//Start = sortArrays [{1,2,3},{2},{2,5,2},{3,1},{1,2},{0}] //[{0},{2},{1,2},{1,2,3},{3,1},{2,5,2}]

//Start = sortArrays [] //[]




:: Q = { nom::Int, den::Int}

half = { nom=1, den=2 }

third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

simplify :: Q -> Q
simplify {nom = n,den = d}
|d == 0 = abort "Error!"
|n== 0 = {nom =0, den =0}
|d < 0 = {nom = ~n/g,den = ~d/g}
= {nom = n/g, den = d/g}
where
 g = gcd n d 
 
instance + Q
  where
  + a b = simplify{nom = a.nom*b.den + b.nom*a.den, den = a.den*b.den}

:: Tree a = Node a (Tree a)(Tree a)	| Leaf

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)

//Write a function that will return the sum of a tree's nodes
//Return the sum as a simplified Q


sumTree :: (Tree Q) -> Q
sumTree Leaf = {nom= 0,den = 1}
sumTree (Node x l r) = x + sumTree l + sumTree r

//Start = sumTree smallTree //{nom=3,den=1}

//Start = sumTree bigTree //{nom=97,den=15}
//Start = sumTree miniTree //{nom=19, den=20}
//Start = sumTree miniTree //{nom=19, den=20}
