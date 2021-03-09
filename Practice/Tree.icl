module Tree
import StdEnv 


::Tree a = Node a (Tree a) (Tree a) | Leaf
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

myTree = Node 3 (Node 2 (NOde 1 (Leaf) (Leaf)) Leaf) (Node 4 Leaf Leaf) 


getnode (Node x l r) = x 
parentFinder Leaf _ = 0
parentFinder (Node x l r) num
|getnode l == num || getnode r == num = x
|num < x = parentFinder l num
|num > x = parentFinder r num



Start = parentFinder ourTree 7
 
 
  




/* 1. Given a list of integers, decide if they are pairwisely relatively prime or not.
(Two integers are relatively prime (or coprime)
if there is no integer greater than one that divides both of them)
*/
//f1::[Int]->Bool
//Start = f1 [] //True
//Start = f1 [1,3,7,9] //False (3,9) not
//Start = f1 [11,12,13] //True


// 2. Define a record type for rational numbers, and add two rational numbers.
::Q = {nom::Int,den::Int}
simplify :: Q ->Q
simplify {nom=n,den=d}
|d==0=abort "Error!"
|d<0={nom = ~n/g,den = ~d/g}
= {nom = n/g, den = d/g}
where g = gcd n d
f0 :: Q Q -> Q
f0 a b = simplify{nom = a.nom*b.den+a.den*b.nom, den = a.den*b.den}
//Start=f2 {nom=2,den=4} {nom=2,den=3} //(Q 7 6)
//Start=f2 {nom=2,den=4} {nom=2,den=4} //(Q 1 1)



/* 3.Given a integer, return a tuple containing
a list of its divisors as the first element of the tuple,
and the mean value of that list as the second element of the tuple.
*/
f3 :: Int -> ([Int],Real)
f3 n = (list,toReal(sum list)/toReal(length list))
where
list = [x\\x<-[1..n]|n rem x==0]
//Start = f3 8 //([1,2,4,8],3.75)
//Start = f3 9 //([1,3,9],4.33333333333333)
//Start = f3 15 //([1,3,5,15],6)




// Given three Vectors in 2D, decide if their endpoint lie on a same line.
// Hint1: Points are on a same line, if area of triangle formed by these points is 0.
// Hint2: https://www.dummies.com/education/math/algebra/finding-the-area-of-a-triangle-using-its-coordinates/
:: Vector2 = {x :: Real, y :: Real}
//collinear :: Vector2 Vector2 Vector2 -> Bool  //(x1y2 + x2y3 + x3y1 – x1y3 – x2y1 – x3y2)/2.
collinear a b c = (a.x*b.y+b.x*c.y+c.x*a.y-a.x*c.y-b.x*a.y-c.x*b.y)/2.0== 0.0
//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 0.0} // True
//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 1.0} // False
Start = collinear {x = 0.0, y = -1.0} {x = 2.0, y = 0.0} {x = 3.0, y = 0.0} // False




// 1. Given three points, write a function which decides if a they form a
// Right Triangle (triangle which has three equal sides).
//f1::Point Point Point -> Bool

//Start = f1 p1 p2 p3
//Start = f1 {x=(-4.0) , y=(-2.0)} {x=(-3.0) , y=(7.0)} {x=(4.0) , y=(-2.0)} //False
//Start = f1 {x=(0.0) , y=(3.0)} {x=(4.0) , y=(0.0)} {x=(0.0) , y=(0.0)} //True



 
//2. Give a list of people of record type Person.
// find how many girls like dog but dislike cat.
//The gender in record Person must be Algebraic type: Gender
/**
:: Gender = Male | Female | Trans
:: Person = {name::String,gender::Gender,likedog::Bool,likecat::Bool}
instance == Gender
where
== Male Male = True
== Female Female =True
== Trans Trans = True
== _ _ =False
f2 :: [Person] ->Int
f2 [] = 0
f2 [a:b]
|a.gender==Female && a.likedog && not(a.likecat)= 1 + f2 b
=0 + f2 b
*/
//Start =f2 [{name = "Alice", gender = Female, likedog = True, likecat = False },{name = "Alice2", gender = Female, likedog = True, likecat = True },{name = "Alice3", gender = Male, likedog = True, likecat = False }] //1
//Start =f2 [{name = "Alice", gender = Female, likedog = True, likecat = False },{name = "Alice2", gender = Female, likedog = True, likecat = False },{name = "Alice3", gender = Male, likedog = True, likecat = False }] //2



//3. find the student with the highest grade
/*
:: Student = {name::String,grade::Real}
f4:: [Student]->Real
f4 list = last gradelist
where
gradelist = sort[x.grade\\x<-list]
*/

//Start = f4 [{name = "Alice", grade = 22.1},{name = "Ban", grade = 58.0},{name = "kiki", grade = 94.2}] //94.2

/*
Implement a function that acts as 'foldr' for
arrays.
*/
//arrFold :: (a -> b -> b) b {a} -> b
arrFold op ini arr = op ini (last list)
where
list = [x\\x<-:arr]
//Start = arrFold (+) 0 {1,2,3,4,5} // 15
// Start = arrFold (++) [] {[1],[2],[3],[4]} \\ [1,2,3,4]
//Start = (+) 4 6


/*
Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2001.11.22

::Person = { name::String
,birthday::(Int,Int,Int)
}
::Tree a = Node a (Tree a) (Tree a)
|Leaf

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
//f5 :: (Tree Person) -> Int
f5 Leaf = Leaf
f5 (Node x l r)
|x.birthday<=(2001,11,22) = Node {name=x.name +++ "_qualify",birthday = x.birthday} (f5 l) (f5 r)
= Node x (f5 l) (f5 r)
//Start = f5 t2 //2
//Start = f5 t3  //3
//Start = {name = "as",birthday = (2010,12,28)}
*/
/*
1. Check if a binary tree is an ordered and balanced tree
(balanced, the difference between the depth of left and right trees is at most 1)
(ordered, nodes on the left subtree < node < nodes on the right subtree)

makelist :: (Tree a) -> [a]
makelist Leaf = []
makelist (Node x l r) = makelist l ++ [x] ++ makelist r
depth :: (Tree Int)-> Int
depth Leaf = 0
depth (Node x l r) = (max (depth l) (depth r)) + 1
balanced :: (Tree Int)-> Bool
balanced (Node x l r)
|depth l - depth r <= 1 = True
|depth r - depth l <=1 =True
=False
f6 :: (Tree Int) -> Bool
f6 Leaf = True
f6 tree = l == sort l && balanced tree
where
l = makelist tree

*/
//Start = f6 (Node 26 (Node 24 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //True
//Start = f6 (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf (Node 28 Leaf Leaf)) Leaf)) //False
//Start = f6 (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf Leaf) (Node 28 Leaf Leaf))) //False
//Start = f6 (Node 26 (Node 31 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //False


//3. Define an abstract type queue

:: Queue a :==[a]
newQueue :: (Queue a)
newQueue = [] // Creates empty queue
isempty :: (Queue a) -> Bool
isempty [] = True
isempty _ = False // Checks if a queue is empty
enqueue :: a (Queue a) -> Queue a
enqueue n [] = [n]// add an item to the queue
enqueue n list = list ++ [n]
dequeue :: (Queue a) -> Queue a //Remove an item  from the queue
dequeue [a:b] = b
peek :: (Queue a) -> a //Gets the element at the front of the queue
peek [a:b] = a


//Start = isempty newQueue //True
//Start =  enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue)) //[(0,9),(6,5),(1,2)]
//Start =  peek (enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue))) //(0,9)
//Start =  dequeue (enqueue (6,5) (enqueue (0,9) newQueue)) //[(6,5)]

/*

:: Month = January | February | March | April | May | June | July | August | September | October | November | December
:: Gender = Male | Female | AttackHelicopter | Nghia
:: Date = {year::Int, month::Month, day::Int}
:: Person = {name::String, gender::Gender, age::Int, birthday::Date, isDead::Bool}
:: Account = {number::Int, owner::Person, balance::Real, dateCreated::Date}

Tringa :: Person
Tringa = {name = "Tringa", gender = Female, age = 42, birthday = {year = 1977, month = May, day = 12}, isDead = False}
Hossam :: Person
Hossam = {name = "Hossam", gender = AttackHelicopter, age = 69, birthday = {year = 1950, month = June, day = 27}, isDead = False}
Nicola :: Person
Nicola = {name = "Nicola", gender = Male, age = 9001, birthday = {year = -6982, month = January, day = 1}, isDead = True}
Zuka :: Person
Zuka = {name = "Zuka", gender = Male, age = 20, birthday = {year = 1999, month = February, day = 11}, isDead = False}
LeMinhNghia :: Person
LeMinhNghia = {name = "Nghia", gender = Nghia, age = 420, birthday = {year = 1599, month = February, day = 4}, isDead = True}

A00 :: Account
A00 = {number = 0, owner = Nicola, balance = 9000.01, dateCreated = {year = 1945, month = August, day = 6}}

A01 :: Account
A01 = {number = 1, owner = Hossam, balance = -1337.42, dateCreated = {year = 1900, month = December, day = 25}}

A02 :: Account
A02 = {number = 2, owner = Tringa, balance = 123.45, dateCreated = {year = 2010, month = May, day = 12}}

A03 :: Account
A03 = {number = 3, owner = Zuka, balance = 35.0, dateCreated = {year = 2019, month = November, day = 11}}

A420 :: Account
A420 = {number = 420, owner = LeMinhNghia, balance = 420.0, dateCreated = {year = 420, month = April, day = 20}}

SittBank :: {Account}
SittBank = {A00,A01,A02,A03}

/*
Write a function that takes a Real and an
array of Accounts updates each Account
in an array by adding the number to the balance.
*/
//add :: Account Real -> Account
//add x raqam = {number=x.number,owner=x.owner,balance=x.balance+raqam,dateCreated=x.dateCreated}
gimmeMoney :: {Account} Real-> {Account}
gimmeMoney bank pesa = {{x & balance = x.balance + pesa}\\x<-:bank}
//Start = gimmeMoney SittBank 1000000.0 //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 1009000.01 (Date 1945 August 6)),(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27) False) 998662.58 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 1000123.45 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) 1000035 (Date 2019 November 11))}
//Start = gimmeMoney SittBank -9999999999999999.9999999999
//{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) -9.999999999991e+15 (Date 1945 August 6)),(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27)
//False) -1.00000000000013e+16 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) -9.99999999999988e+15 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) -9.99999999999996e+15 (Date 2019 November 11))}

/*
Write a function that takes an array of Accounts
and returns the name of the owner with the
highest balance who is NOT dead.
*/

soRich :: {Account} -> String
soRich arry = last[x.owner.name\\x<-:arry | not(x.owner.isDead) && x.balance>=high]
where
list = [x.balance\\x<-:arry|not(x.owner.isDead)]
high = last (sort list)
//Start = soRich {A01, A02, A03} //"Tringa"
//Start = soRich SittBank //"Tringa"

//Start= {1:{2}}

/*
Write a function that takes a tuple containing a
condition and two numbers, and an array of Accounts,
and returns an array containing all accounts
that match the condition, and that were created between
the two years (numbers provided).
*/
instance == Gender
where
== Male Male = True
== Female Female =True
== AttackHelicopter AttackHelicopter = True
== Nghia Nghia = True
== _ _ = False
query :: ((Account -> Bool),Int,Int) {Account} -> {Account}
query (cond,str,end) arry = {x\\x<-:arry | cond x && x.dateCreated.year >=str && x.dateCreated.year <=end}
//Start = query ((\x = x.owner.gender==Male),0,2000) SittBank //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 9000.01 (Date 1945 August 6))}
//Start = query ((\x = x.balance > 0.0),-9999,9999) SittBank //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 9000.01 (Date 1945 August 6)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 123.45 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) 35 (Date 2019 November 11))}
//Start = query ((\x = not x.owner.isDead && length[z\\z<-:x.owner.name]==6),0,2020) {A00,A01,A02,A03,A420} //{(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27) False) -1337.42 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 123.45 (Date 2010 May 12))}
*/



:: Tree a = Node a (Tree a) (Tree a) | Leaf
:: Yggdrasil a b c = Core a (Tree b) {c}

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

oddTree :: (Tree Int)
oddTree = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 9 Leaf Leaf) Leaf))

flow1 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow1 = Core and ourTree {isEven, ((<)10)}


flow2 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow2 = Core or oddTree {((<)10),((==)1),(\x = and[x rem n <> 0\\n<-[2..(x-1)]])}

/*
Write an instance of equality (==) between two Trees,
such that we can use calls such as treeA == treeB.
*/
instance == (Tree a) | Eq a
where
== Leaf Leaf = True
== Leaf _ = False
== _ Leaf = False
== (Node x l r) (Node y m n) = (x==y) && (l == m) && (r == n)
//Start = ourTree == ourTree //True
//Start = ourTree == oddTree //False
//Start = isMember ourTree [ourTree, oddTree, Leaf] //True

/*
Write a function that will take the data constructs
and apply the array of conditions in the array portion,
and apply it with the boolean aggregator function (first portion)
to every element of the tree (second portion),
returning a list of elements that return True after processing.
For example:
Given a data construct of Core or someTree {cond1,cond2,cond3,cond4},
then return a list of elements from someTree, given that they return
True for one or more (the 'or' function) of the conditions from the
array (cond1, cond2, etc...).
Hint: Arrays are easier handled as lists for some operations.
*/
makelist :: (Tree a) -> [a]
makelist Leaf = []
makelist (Node x l r) = makelist l ++ [x] ++ makelist r

flowApp :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool)) -> [Int]
flowApp (Core op tree arr)= [x\\x<-(makelist tree) | op[y x\\y<-:arr]]
//Start = flowApp flow1 //[18,20,24,26,28]
//Start = flowApp flow2 //[1,3,5,7,11]


:: Complex = { real :: Real, imaginary :: Real}

:: RoflCopter :== Complex

timesRofl :: RoflCopter Int -> RoflCopter
timesRofl x n = {real=x.real * (toReal n), imaginary = x.imaginary * (toReal n)}

/*
Given the above defined abstract data type, record, and function:
you are given a task to expand functionality by making RoflCopter
a Complex.
Refactor the code, so we can multiply the new RoflCopter by an Int.
Definition: Refactor (verb)
"a disciplined technique for restructuring an existing body of code,
altering its internal structure without changing its external behavior.
Its heart is a series of small behavior preserving transformations."
That is, make the code work, without changing the arguments of timesRofl.
You will need to change RoflCopter's definition, and the internal workings
of timesRofl.
*/
//Start = timesRofl {real = 2.0, imaginary = 3.0} 5 //(Complex 10 15)
//Start = timesRofl {real = (acos -1.0), imaginary = (exp 1.0)} 100 //(Complex 314.159265358979 271.828182845905)
//Start = timesRofl {real = 0.0, imaginary = 0.0} 420 //(Complex 0 0)

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
addEntry [] st a = [(st,a)]
addEntry [(key,val):b] str n
|key==str=[(key,val):b]
=[(key,val)]++addEntry b str n
//Start = myDict
//Lookup the value associated with a key.
//Return 0 if the key isn't found.
(lookup) :: Dictionary String -> Int
(lookup) [] _ = 0
(lookup) [(key,val):b] str
|key==str = val
= (lookup) b str

//Find all keys associated with a certain value.
//Return as a list of keys.
(findKeys) :: Dictionary Int -> [String]
(findKeys) [] _ = []
(findKeys) [(key,val):b] n
|val==n = [key: (findKeys) b n]
= (findKeys) b n
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


//Given these Algebraic Data Types, Records, and Tree...
:: Gender = Male | Female | NonBinary | AttackHelicopter | Nghia | OOBLECK
:: LivingStatus = Alive | Deceased | Undead
:: MarriageStatus = Married | Divorced | Single | Tinder
:: Person = { name :: String, gender :: Gender, age :: Int, livingStatus :: LivingStatus, marriageStatus :: MarriageStatus}
:: FamilyTree a = Name a (FamilyTree a) (FamilyTree a) | End | Polygamy [[[[[[[[[[[[[FamilyTree a]]]]]]]]]]]]]

//And these people:
Olivia = {name = "Olivia", gender = Female, age = 19, livingStatus = Alive, marriageStatus = Single}
Amelia = {name = "Amelia", gender = Female, age = 83, livingStatus = Alive, marriageStatus = Married}
Isla = {name = "Isla", gender = Female, age = 40, livingStatus = Alive, marriageStatus = Married}
Emily = {name = "Emily", gender = Female, age = 73, livingStatus = Alive, marriageStatus = Divorced}
Ava = {name = "Ava", gender = Female, age = 18, livingStatus = Alive, marriageStatus = Single}
Lily = {name = "Lily", gender = Female, age = 50, livingStatus = Alive, marriageStatus = Divorced}
Oliver = {name = "Oliver", gender = Male, age = 56, livingStatus = Alive, marriageStatus = Married}
Harry = {name = "Harry", gender = Male, age = 45, livingStatus = Alive, marriageStatus = Married}
Jack = {name = "Jack", gender = Male, age = 90, livingStatus = Deceased, marriageStatus = Married}
George = {name = "George", gender = Male, age = 43, livingStatus = Alive, marriageStatus = Married}
Noah = {name = "Noah", gender = Male, age = 74, livingStatus = Undead, marriageStatus = Divorced}
Freddie = {name = "Freddie", gender = Male, age = 24, livingStatus = Alive, marriageStatus = Single}
Ethan = {name = "Ethan", gender = Male, age = 20, livingStatus = Alive, marriageStatus = Single}

//And each person's immediate parents:
OliviaTree = Name Olivia OliverTree HarryTree
OliverTree = Name Oliver End End
HarryTree = Name Harry AmeliaTree JackTree
AmeliaTree = Name Amelia End End
JackTree = Name Jack End End
EthanTree = Name Ethan GeorgeTree IslaTree
GeorgeTree = Name George AmeliaTree JackTree
IslaTree = Name Isla NoahTree EmilyTree
NoahTree = Name Noah End End
EmilyTree = Name Emily End End
AvaTree = Name Ava LilyTree OliverTree
LilyTree = Name Lily End End
FreddieTree = Name Freddie End End

personsList = [Olivia, Amelia, Isla, Emily, Ava, Lily, Oliver, Harry, Jack, George, Noah, Freddie, Ethan]
familyList = [OliviaTree, OliverTree, HarryTree, AmeliaTree, JackTree, EthanTree, GeorgeTree, IslaTree, NoahTree, EmilyTree, AvaTree, LilyTree, FreddieTree]

/*
Write a function that tests if two persons are cousins
Condition: They share a grandparent.
*/
getperson (Name x _ _) = x
maapay :: Person -> [Person]
maapay baal = flatten[[getperson m]++[getperson b]\\(Name x m b)<-familyList | baal.name==x.name && baal.age == x.age]
dadidada :: Person -> [Person]
dadidada baal = flatten[maapay x\\x<-(maapay baal)]
//Start= dadidada Ethan


(equal) a b = a.name==b.name && a.age==b.age
(isfamily) (Name x l r) p
|p equal x = True
=False
getfamily baal = hd[x\\x<-familyList | x isfamily baal]
getparents a = get2 (getfamily a)
get2 (Name a End End) = []
get2 (Name x l r)= [getperson l,getperson r]

getgrand a = flatten[getparents x\\x<-(getparents a)]
//getgrand baal = [getparents x\\x<-(getparents)]
//areCousins :: Person Person -> Bool
areCousins a b
|a equal b = False
|or[m equal n\\m<-(getparents a),n<-(getparents b)]== True = False
= or[x equal y\\x<-(getgrand a),y<-(getgrand b)]
//Start = areCousins Ethan Olivia //True
//Start = areCousins Ethan Ava //False
//Start = areCousins George Harry //False
//Start = areCousins George Isla //False
//Start = areCousins Ethan Ethan //False (same person)

instance == Gender
where
== Male Male = True
== Female Female  = True
== _ _ = False
//1. Write a function that tests if a person is adopted.
//Condition: Person has two parents of same gender, we're simplifying here.
//isAdopted :: Person -> Bool
isAdopted baal = isreally(getparents baal)
isreally [] = False
isreally [a,b]= a.gender == b.gender

//Start = isAdopted Olivia //True
//Start = isAdopted Ethan //False
//Start = isAdopted Freddie //False (we don't know who his parents are)


/*
Below are functions that will break up
a string into  the individual words, and
put them back together again.
*/
/*
parse :: String -> [String]
parse s
| size s == 0 = [] //if we have no letters left, we stop recursion.
= [word : parse rest]
where
    l = [x\\x<-:s] //convert string to list.
    (w,r) = span (\x = x <> ' ') l
    //span combines takeWhile and dropWhile to give us the first word and everything else.
    word = {x\\x<-w} //converting the first word back to a string.
    rest
    | isEmpty r = "" //handles the end case.
    = {x\\x<-(tl r)} //converting the rest back to a string for recursion. tl to remove the ' ' character.
//Start = parse "Joy to the World!" //["Joy","to","the","World!"]

concatStrings :: [String] -> String
concatStrings list = foldr (\ x y = x +++ " " +++ y) "" list
//Start = concatStrings ["Joy","to","the","World!"]
*/
puttolist :: String -> [String]
puttolist str
|size str == 0 =[]
= [lafz]++ puttolist baqi
where
list = [x\\x<-:str]
(a,b) = span (\x = x<>' ') list
lafz = {x\\x<-a}
baqi
|isEmpty b = ""
= {x\\x<-(tl b)}
//Start = puttolist "Joy to the World!"
outlist :: [String] -> String
outlist list = foldr (\x y = x +++ " " +++ y) "" list
//Start = outlist ["Joy","to","the","World!"]0

//Start = splitAt 7 [1,2,3,4,5]
qsort [] = []
qsort [a:xs] = qsort[x \\ x <- xs | x > a] ++ [a] ++ qsort[x \\ x <- xs| x <= a]
l :: {Int}
l = {1,2,3,4,5,6}