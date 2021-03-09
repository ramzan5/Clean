module Abdullah
import StdEnv


//Write a function that takes a binary search tree and a list of integers and adds the integers
//into the tree, in the order given by the list. Duplicates are not allowed here.
//Note: order of elements in the list changes the resulting tree.
//You must define the Tree type.

shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
//addList :: [Int] (Tree Int) -> (Tree Int)
:: Tree a = Node a (Tree a) (Tree a) | Leaf
//Start = addList [1,10,14,20] shortTree //(Node 14 (Node 11 (Node 1 Leaf (Node 10 Leaf Leaf)) (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) (Node 20 Leaf Leaf)))
//Start = addList [20,10,1,14] shortTree //(Node 14 (Node 11 (Node 10 (Node 1 Leaf Leaf) Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) (Node 20 Leaf Leaf)))
//Start = addList [2,3,2,1] (Node 3 Leaf Leaf) //(Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf)
//Start = addList [2,5,1,4,3] Leaf //(Node 2 (Node 1 Leaf Leaf) (Node 5 (Node 4 (Node 3 Leaf Leaf) Leaf) Leaf))
//Start = addList [1,2,3,4,5] Leaf //(Node 1 Leaf (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf (Node 5 Leaf Leaf)))))
//Start = addList [] shortTree //(Node 14 (Node 11 Leaf (Node 13 Leaf Leaf)) (Node 17 (Node 15 Leaf Leaf) Leaf))
//Start = addList [] Leaf //Leaf

/*
Implement the Dictionary data type.
A dictionary is a list containing
pairings of two entries.
The first entry is the "key".
The second entry is the "value".
Every key has an value associated with it.
Keys are unique, there can not be duplicates.
Values can be duplicated between keys.
*/

//Type definition.
:: Dictionary :== [(String,Int)]
//Custom Dictionary for testing.

myDict :: Dictionary
myDict = addEntry (addEntry (addEntry (addEntry (addEntry (addEntry (addEntry newDict "Evan" 42) "Hossam" 69) "Nghia" 420) "Tringa" 1337) "Zuka" 13) "Nikola" 9001) "Ying" 420

newDict :: Dictionary
//func list
newDict = []
//Start = hd myDict
addEntry :: Dictionary String Int -> Dictionary
addEntry dict x y
| not (isMember x [key\\(key,value)<-dict]) = dict ++ [(x,y)]
= dict



//Find all keys associated with a certain value.
//Return as a list of keys.
(findKeys) :: Dictionary Int -> [String]

(findKeys) dict num = [key\\(key,value)<-dict | value == num]


//Start = myDict findKeys 42 //["Evan"]
//Start = myDict findKeys 420 //["Nghia","Ying"]
//Start = myDict findKeys 99999999999 //[]















/*



//Type definition.
:: Dictionary :== [(String,Int)]
//Convenient toString instance:
instance toString Dictionary
where
    toString [] = "End of Dictionary"
    toString [(key,value):rest] = "Key: "+++(toString key)+++", "+++"Value: "+++(toString value)+++"\n"+++(toString rest)
//Custom Dictionary for testing.

myDict:: Dictionary
myDict = addEntry (addEntry (addEntry (addEntry (addEntry (addEntry (addEntry newDict "Evan" 42) "Hossam" 69) "Nghia" 420) "Tringa" 1337) "Zuka" 13) "Nikola" 9001) "Ying" 420


//Create a new dictionary. Should be empty.
newDict :: Dictionary
newDict = []
//Add a new entry. Be sure to check for duplicate keys!
addEntry :: Dictionary String Int -> Dictionary
addEntry [] str num = [(str,num)]
addEntry [(key, value): rest] str num 
|key == str = [(key, value): rest]
= [(key, value)] ++ addEntry rest str num


//Lookup the value associated with a key.
//Return 0 if the key isn't found.
(lookup) :: Dictionary String -> Int
(lookup) [] str = 0
(lookup) [(key,value):rest] str 
|key == str = value
= (lookup) rest str

//Find all keys associated with a certain value.
//Return as a list of keys.
(findKeys) :: Dictionary Int -> [String]
(findKeys) dict num = [key\\(key,value)<-dict | value == num]

//Start = toString newDict //"End of Dictionary"
//Start = toString myDict
/*
"Key: Evan, Value: 42
Key: Hossam, Value: 69
Key: Nghia, Value: 420
Key: Tringa, Value: 1337
Key: Zuka, Value: 13
Key: Nikola, Value: 9001
Key: Ying, Value: 420
End of Dictionary"
*/
//Start = toString(addEntry myDict "Hossam" 0)
/*
"Key: Evan, Value: 42
Key: Hossam, Value: 69
Key: Nghia, Value: 420
Key: Tringa, Value: 1337
Key: Zuka, Value: 13
Key: Nikola, Value: 9001
Key: Ying, Value: 420
End of Dictionary"
*/
//Start = myDict lookup "Nikola" //9001
//Start = myDict lookup "Viktoria" //0
//Start = myDict findKeys 42 //["Evan"]
//Start = myDict findKeys 420 //["Nghia","Ying"]
//Start = myDict findKeys 99999999999 //[]

*/




