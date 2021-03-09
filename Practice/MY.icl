module MY
import StdEnv

// Solve as many functions as you can. Each exercise is of 10%, to pass min. 40% is necessary.
// marks: 40%-2,60%-3,80%-4,100%-5.
//1.
//Define a tree type and use the followings for testing your solution.
:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf
//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].
/*geta :: (Tree a) -> a
geta (Node x l r) = x
isPrime n
|n<=1 = False
= isEmpty[x\\x<-[2..(n-1)] | n rem x==0]
isleaf Leaf = True
isleaf _ = False
primeChildren :: (Tree Int) -> [Int]
primeChildren Leaf = []
primeChildren (Node _ Leaf Leaf) = []
primeChildren (Node x l r)
|isPrime (geta l) || isPrime (geta r)  = [x]++ (primeChildren l) ++ (primeChildren r)
= (primeChildren l) ++ (primeChildren r)
*/
ExtractNode :: (Tree a)->a
ExtractNode (Node x l r) = x
//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].
isleaf Leaf = True
isleaf _ = False
/*isPrime :: Int->Bool
isPrime a
|a<=1= False
= isEmpty[x\\x<-[2..(a-1)] | a rem x ==0]
primeChildren :: (Tree Int) -> [Int]
primeChildren Leaf = []
*/
primeChildren (Node n l r)
|isleaf l || isleaf r = []
|isPrime (ExtractNode l) || isPrime (ExtractNode r) =  [n] ++ primeChildren l  ++ primeChildren r
= primeChildren l ++ primeChildren r
Start = primeChildren tree1 //[10,7]
//Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]



//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (a,b) = [x\\x<-(union) | not (isMember x inter)]
where
union = [x\\x<-:a]++[y\\y<-:b]
inter = [x\\x<-:a | isMember x [y\\y<-:b]]

//Start = symmetricDiff ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
//Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
//Start = symmetricDiff ({},{}) //[]





//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. Simplify the fraction before returning.
//In case the nominator is zero, set the denominator to zero as well.

:: Q = {nom :: Int, den :: Int}

simplify :: Q -> Q
simplify = {nom = n,den = d}
|d == 0 = abort "Error!"
|n== 0 = {nom =0, den =0}
|d < 0 = {nom = ~n/g,den = ~d/g}
= {nom = n/g, den = d/g}
where
g = gcd n d  

instance / Q
where
/ a b = {nom = (a.nom * b.den), den = (b.nom * a.den)}
fracDivision :: Q Q -> Q
//fracDivision x y = simplify x/y
fracDivision a b = simplify (a/b)// {nom=a.nom*b.den,den=a.den*b.nom}

//Start = fracDivision {nom=5, den=1} {nom=6, den=5} //{nom=25, den=6}
//Start = fracDivision {nom=10, den=2} {nom=3, den=4} //{nom=20, den=3}
//Start = fracDivision {nom=0, den=2} {nom=100, den=4} //{nom=0, den=1}
//Start = fracDivision {nom=15, den=2} {nom=3, den=12} //{nom=30, den=1}

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

//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.
instance + Q
where
+ a b = simplify{nom = a.nom*b.den + b.nom*a.den, den = a.den*b.den}

sumTree :: (Tree Q) -> Q
sumTree Leaf = {nom= 0,den = 1}
sumTree (Node x l r) = x + sumTree l + sumTree r

//Start = sumTree smallTree //{nom=3,den=1}
//Start = sumTree bigTree //{nom=97,den=15}
//Start = sumTree miniTree //{nom=19, den=20}

//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.

treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r

instance == Q
where
    == x y = x.nom * y.den == x.den * y.nom

instance < Q
where
    < x y = x.nom * y.den < y.nom * x.den

checkTree :: (Tree Q) -> Bool
checkTree tree = (treeToList tree) == sort(treeToList tree)

//Start = checkTree bigTree //True
//Start = checkTree smallTree //True
//Start = checkTree badTree //False
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

//8.
//Amicable numbers are two different numbers so related that the sum of the proper divisors of each
//is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself.
//For example, the proper divisors of 6 are 1, 2, and 3.)
//Check if two integers are amicable pairs and put them together with the answer in a bag.
//amicable pair: 1184 and 1210
//proper divisor of 1184 :  1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592 -> their sum == 1210
//proper divisor of 1210 : 1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605 -> their sum == 1184

properDiv :: Int -> [Int]
properDiv n = [x\\x<-[1..(n-1)]|n rem x == 0]

:: Bag a :==[((Int,Int),Bool)]
(amic) :: Int Int -> Bool
(amic) x y = (sum listx) == y && (sum listy)==x
where
listx = [a\\a<-[1..(x-1)] | x rem a == 0]
listy = [b\\b<-[1..(y-1)] | y rem b == 0]
amiBag :: [(Int,Int)] -> Bag a
amiBag list = [((x,y),x amic y)\\(x,y)<-list|x amic y]


//Start = amiBag [(1184,1210), (13,245)]
//Start = amiBag [] // []
//all true
//Start = amiBag [(220, 284), (1184, 1210), (2620, 2924), (5020, 5564), (6232, 6368), (10744, 10856), (12285, 14595), (17296, 18416), (63020, 76084), (66928, 66992)]

//9.
//Given the Object type, compute for the state component the given method and print the result as a String.
//ex: for state 3 compute 3 + 1 using the given method, and print the result "4" as string.
:: Object = {state::Int, method::Int->Int, tostring:: Int -> String }
MyObject = {state = 3 , method = (+) 1, tostring = toString}
//Start = MyObject.tostring (MyObject.method MyObject.state)
//The above line is the solution. It should print "4". Unfortunately the question isn't well written.

//10.
//Given an operator and two lists, apply the operator to the elements of
//the same positions of lists, like in the examples.
:: Operator a :== a a -> a

op2 :: (Operator a) [a] [a] -> [a]
op2 op x y = [op a b\\a<-x & b<-y]
//Start = op2 (*) [2,3,4,5] [7,8,9,10] // [14,24,36,50]
//Start = op2 (*) [2,3,4,5] [7,8] // [14,24]
//Start = op2 (*) [2,3] [7,8,9,10] // [14,24]
//Start :: [Int]
//Start = op2 (*) [] [] // []
//Start = op2 (+) [3,2] [7,8] // [10,10]





// A Pythagorean triad is a triple of integers (a,b,c) such that
// a^2 + b^2 == c^2
// Count how many triads are there with 1<=a<=b<=c<=n.
// n is given as parameter
// Hint/Requirement: Use list comprehension
// Hint: To avoid three loops, check if a*a + b*b is perfect square, if yes increase count.
// Also, you have to check that sqrt(a * a + b * b) <= n. No need to check sqrt(a * a + b * b)>=a,b, it is automatic. Think why.
perf n = (sqrt (toReal n)) == toReal(toInt(sqrt (toReal n)))
countAll :: Int -> Int
countAll n = length[(x,y,toInt(sqrt(toReal (x^2+y^2))))\\x<-[1..n] , y<-[x..n]|perf (x^2+y^2) && toInt(sqrt(toReal (x^2+y^2))) <= n ]

// xlist = [x\\x<-[1..n],y<-[x..n] | perf (x^2 + y^2)( && (x^2 + y^2) <= n]
//Start = perf 9
//Start = countAll 100 // 52
//Start = countAll 1442 // 1349
//Start = countAll 3134 // 3334

// You are given a point and triangle. Determine if point lies inside triangle.
// Hint: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
// You have to write records for Point and Triangle. Point should be represented as two Real coordinates x and y.
// Triangle is represented as three Points a,b and c.
/*:: Point = {x::Real,y::Real}
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
*/
//Start = isInside {x = 0.0, y = 0.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // True
//Start = isInside {x = 3.0, y = 4.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // False
//Start = isInside {x = 0.0, y = 0.0} {a = {x = 0.0, y = 0.0}, b = {x = 0.0, y = 0.0}, c = {x = 0.0, y = 0.0}} // True
//Start = isInside {x = 0.0, y = 1.0} {a = {x = -1.0, y = 1.0}, b = {x = 1.0, y = 1.0}, c = {x = 1.0, y = 1.0}} // True




//  It is hypothesized that every even number greater than two can be expressed as the sum of two primes.
//  For example, 4 = 2+2, 6 = 3+3, 8 = 3+5.
//  Check if this is true for all even numbers in the range 4 to n?
//  Hint/Requirement: Use list comprehension
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

//Start = check 1000 // True
//Start = check 100000 // True
//Start = check 500 // True
//Start = check 10 // True


// 1. Given three points, write a function which decides if a they form a
// Right Triangle (triangle which has three equal sides).
:: Point = {x::Real,y::Real}
f10::Point Point Point -> Bool
f10 p q r
|d1 + d2 == d3 = True
|d2 + d3 == d1 = True
|d1 + d3 == d2 = True
=False
where
d1 = ((q.x-p.x)^2.0 + (q.y-p.y)*(q.y-p.y))
d2 = ((r.x-q.x)*(r.x-q.x) + (r.y-q.y)*(r.y-q.y))
d3 = ((p.x-r.x)*(p.x-r.x) + (p.y-r.y)*(p.y-r.y))
//Start = f10 p1 p2 p3
//Start = f10 {x=(-4.0) , y=(-2.0)} {x=(-3.0) , y=(7.0)} {x=(4.0) , y=(-2.0)} //False
//Start = f10 {x=(0.0) , y=(3.0)} {x=(4.0) , y=(0.0)} {x=(0.0) , y=(0.0)} //True




/*
My friends and I went to play football in the streets, and the game ended in a tie,
so we were discussing if we should go for penalties or not, please help me decide that.
You will get each one of my team skill Level and name in a list,
and you will get the name of the other team's goalkeeper and his/her level of skill.
If the skill of the player is more or equal than the skill of the goalkeeper
it will count as scored penalty, because my team's goalkeeper is super skillfull,
we should score 3 penalties or more to win this virtual game.
*/
::Player = { name :: String, skillLevel :: Int}
shouldWePlay :: [Player] Player -> Bool
shouldWePlay plyrs keepr = length list >=3
where
list = [x\\x<-plyrs | x.skillLevel >= keepr.skillLevel]
//Start = shouldWePlay [{name = "kareem", skillLevel = 4},{name = "Tarek", skillLevel = 3},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // False
//Start = shouldWePlay [{name = "kareem", skillLevel = 5},{name = "Tarek", skillLevel = 4},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // True


/*
A teacher in a high school is trying to choose the student of the year in his school,
so let's help him doing that. The teacher wants the student to have a higher gpa.
The teacher is adding 0.1 to the GPA if the student was good.
*/
::Status = Good | Bad
::Pupil = {student_name :: String, gpa :: Real, status :: Status}
instance == Status
where
== Good Good = True
== Bad Bad = True
== _ _  = False
bestStudent :: [Pupil] -> Pupil
bestStudent [a:b]
|a.gpa == most && a.gpa < 4.0 && a.status == Good = {a & gpa = a.gpa + 0.1 }
|a.gpa == most && a.gpa >= 4.0 && a.status == Good = a
= bestStudent b
where
list = [x.gpa\\x<-[a:b]]
most = last (sort list)
//Start = bestStudent [{student_name = "Khalid Walid", gpa = 4.0, status = Good},{student_name = "Peter", gpa = 3.7, status = Good},{student_name = "Yoko", gpa = 2.9, status = Bad}] // {student_name = "Khalid Walid", gpa = 4.0, status = Good}
//Start = bestStudent [{student_name = "Khalid Walid", gpa = 3.7, status = Bad},{student_name = "Peter", gpa = 3.7, status = Good},{student_name = "Yoko", gpa = 2.9, status = Bad}] // {student_name = "Peter", gpa = 3.8, status = Good}

/*
Given an array find the maximum value and return new array which has all occurrences of
the maximum value removed.
For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, maximum is 5,
so answer should be {1,4,3,3,2,4,1,3,4}.
*/
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


/*
Given a list of Integer arrays, your task is to sum up all of them and return
a new array. (Sum first elements of arrays, second elements of arrays and so on).
You can assume that all arrays have the same length.
*/
//summ [a:b] = [x+y\\x<-a & y<-(hd b)] ++ summ b
//sumArrays :: [{Int}] -> {Int}
//sumArrays :: [{Int}] -> [Int]
//sumArrays [] = []
//sumArrays [a:b] =  ([x\\x<-:a] ++ sumArrays b)
//Start = summ [[1,2],[1,2]]
//Start = sumArrays [{1,2,3}, {1,2,3}] // {2,4,6}
// Start = sumArrays [{}, {}] // {}
// Start = sumArrays [{1}, {5}] // {6}
// Start = sumArrays [{1,0,0}, {0,1,0}, {0,0,1}] // {1,1,1}


::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}


/*
Given a University, return an array of all the
students names which have gpa greater than 4,
and a favorite teacher who teaches Functional
*/
//Start = avg [1,2,3]
//Start = Josh
list :: {Int} -> [Int]
list arr = [x\\x<-:arr]

gpaAndFavoriteTeacher::University->{String}
gpaAndFavoriteTeacher uni = {x.studentName\\x<-uni.students| avg (list x.grades) >= 4 && x.favoriteTeacher.subject == "Functional"}
//Start=gpaAndFavoriteTeacher BMI//{"Josh","Sofi"}
//Start=gpaAndFavoriteTeacher ELTE//{"Josh"}
//Start=gpaAndFavoriteTeacher EmptyUni//{}


/*
Write an instance of type Student, such that two students are equal
if their gpa differs in less than 0.3 and they have the same favorite teacher
*/
/*
instance == Student
where
(==) :: !Student !Student -> Int
(==) std1 std2  = (avg (list std1.grades)) -  (avg (list std2.grades))// == 0.3 =True  
//|toReal(avg (list std2.grades)) -  toReal(avg (list std1.grades))// == 0.3 =True  
//= 0.0 //False
*/
//Start= Nikola == Nik//True
//Start=Nikola == Nikola //True
//Start= Nik== Nik2//False
//Start= Nikola == Nik2//False


/*
Create an * instance of lists such that list1 * list2 will give a
list of pairwise product of the two lists and if the length
of one list is greater than the other one just add the
remaining elements to the end of the new list
*/
biglist m n
|length m >length n =m
|length n > length m =n
instance * [a] | * a
where
* x y
|length x == length y = [a*b\\a<-x & b<-y]
= list ++ drop (length list) (biglist x y)
where
list = [a*b\\a<-x & b<-y]
//Start=[1,2]*[3,4,5,6,0]//[3,8,5,6,0]
//Start= [1,2,3,1,3,12,312] *[2,3]//[2,6,3,1,3,12,312]
//Start :: [Int]
//Start= [] * []//[]

:: Gender = Male | Female | Nghia | AttackHelicopter | OOBLECK
:: Person = {givenName :: String, lastName :: String, gender :: Gender}
:: FamilyTree = Name Person FamilyTree FamilyTree | End | Polygamy [[[[[[[[[[[FamilyTree]]]]]]]]]]]

instance == Gender
where
    == Male Male = True
    == Female Female = True
    == Nghia Nghia = True
    == AttackHelicopter AttackHelicopter = True
    == OOBLECK OOBLECK = True
    == _ _ = False

instance == Person
where
    == p1 p2 = and[p1.givenName == p2.givenName, p1.lastName == p2.lastName, p1.gender == p2.gender]

instance == FamilyTree
where
    == End End = True
    == (Name x1 l1 r1) (Name x2 l2 r2) = and[ x1==x2, l1==l2, r1==r2]
    == _ _ = False

Pedro :: Person
Pedro = {givenName = "Pedro Henrique", lastName = "Villar deFigueiredo", gender = Male}
Mauro :: Person
Mauro = {givenName = "Mauro", lastName = "daRocha Carvalho", gender = Male}
Joao :: Person
Joao = {givenName = "Joao", lastName = "Pereira Cavalcanti", gender = Male}
Carlos :: Person
Carlos = {givenName = "Carlos", lastName = "Teixeira deAndrade", gender = Male}
Luiz :: Person
Luiz = {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male}
Leoberto :: Person
Leoberto = {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male}
Luan :: Person
Luan = {givenName = "Luan", lastName = "deRosas Lima", gender = Male}
Matheus :: Person
Matheus = {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male}
AnaMaria :: Person
AnaMaria = {givenName = "Ana Maria", lastName = "Silva Figueira", gender = Female}
Lucia :: Person
Lucia = {givenName = "Lucia", lastName = "Elena Paiva", gender = Female}
Elena :: Person
Elena = {givenName = "Elena Maria", lastName = "Lacerda Leite", gender = Female}
Vitoria :: Person
Vitoria = {givenName = "Vitoria", lastName = "Correia Negrao", gender = Female}
Miriam :: Person
Miriam = {givenName = "Miriam", lastName = "Marinho Silva", gender = Female}
Veronica :: Person
Veronica = {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female}
Olivia :: Person
Olivia = {givenName = "Olivia", lastName = "Alves daSilva", gender = Female}
Maria :: Person
Maria = {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female}
Bruna :: Person
Bruna = {givenName = "Bruna", lastName = "Melo Guedes", gender = Female}

PedroFamily :: FamilyTree
PedroFamily = Name Pedro MauroFamily LuciaFamily

MauroFamily :: FamilyTree
MauroFamily = Name Mauro JoaoFamily ElenaFamily
LuciaFamily :: FamilyTree
LuciaFamily = Name Lucia CarlosFamily VitoriaFamily

JoaoFamily :: FamilyTree
JoaoFamily = Name Joao LuizFamily MiriamFamily
ElenaFamily :: FamilyTree
ElenaFamily = Name Elena LeobertoFamily VeronicaFamily
CarlosFamily :: FamilyTree
CarlosFamily = Name Carlos LuanFamily OliviaFamily
VitoriaFamily :: FamilyTree
VitoriaFamily = Name Vitoria MatheusFamily MariaFamily

LuizFamily :: FamilyTree
LuizFamily = Name Luiz End End
MiriamFamily :: FamilyTree
MiriamFamily = Name Miriam End End
LeobertoFamily :: FamilyTree
LeobertoFamily = Name Leoberto End End
VeronicaFamily :: FamilyTree
VeronicaFamily = Name Veronica End End
LuanFamily :: FamilyTree
LuanFamily = Name Luan End End
OliviaFamily :: FamilyTree
OliviaFamily = Name Olivia End End
MatheusFamily :: FamilyTree
MatheusFamily = Name Matheus End End
MariaFamily :: FamilyTree
MariaFamily = Name Maria End End

fixedTree :: FamilyTree
fixedTree = (Name {givenName = "Pedro Henrique", lastName = "Lima Mourao", gender = Male} (Name {givenName = "Mauro", lastName = "Santos Mourao", gender = Male} (Name {givenName = "Joao", lastName = "Silva Mourao", gender = Male} (Name {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male} End End )(Name {givenName = "Miriam", lastName = "Marinho Silva", gender = Female} End End ))(Name {givenName = "Elena Maria", lastName = "deCarvalho Santos", gender = Female} (Name {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male} End End )(Name {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female} End End )))(Name {givenName = "Lucia", lastName = "Duarte Lima", gender = Female} (Name {givenName = "Carlos", lastName = "daSilva Lima", gender = Male} (Name {givenName = "Luan", lastName = "deRosas Lima", gender = Male} End End )(Name {givenName = "Olivia", lastName = "Alves daSilva", gender = Female} End End ))(Name {givenName = "Vitoria", lastName = "Pordeus Duarte", gender = Female} (Name {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male} End End )(Name {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female} End End ))))

/*
Would you kindly write a function that takes
a FamilyTree and returns the FamilyTree with
the last names of everyone fixed.


In a FamilyTree, the root is the child,
and the left and right nodes are the parents.

Everyone here is Brazilian.
Each person's last name has two parts.
The first part comes from the mother's last name's second part,
the second part comes from the father last name's second part.

For example, if the parents are:
Mother -> Olivia Alves daSilva
Father -> Mauro daRocha Carvalho
Child -> Pedro daSilva Carvalho

If the Child has no parents, then do not fix their last name.
For simplicity sake, assume every child will have 2 Parents or none.
*/
//fixLastNames :: FamilyTree -> FamilyTree
//Start = fixLastNames PedroFamily == fixedTree //True

:: Router = { nodeName :: String, activeStatus :: Bool}
:: Network = Node Router Network Network | Termination

r1 :: Router
r1 = {nodeName = "PL1", activeStatus = True}
r2 :: Router
r2 = {nodeName = "PL2", activeStatus = True}
r3 :: Router
r3 = {nodeName = "PL3", activeStatus = False}
r4 :: Router
r4 = {nodeName = "PL4", activeStatus = True}
r5 :: Router
r5 = {nodeName = "PL5", activeStatus = False}
r6 :: Router
r6 = {nodeName = "PL6", activeStatus = True}
r7 :: Router
r7 = {nodeName = "PL7", activeStatus = True}
r8 :: Router
r8 = {nodeName = "PL8", activeStatus = False}
r9 :: Router
r9 = {nodeName = "PL9", activeStatus = False}
r10 :: Router
r10 = {nodeName = "PL10",  activeStatus = False}
r11 :: Router
r11 = {nodeName = "PL11",  activeStatus = True}
r12 :: Router
r12 = {nodeName = "PL12",  activeStatus = True}
r13 :: Router
r13 = {nodeName = "PL13",  activeStatus = False}
r14 :: Router
r14 = {nodeName = "PL14",  activeStatus = True}
r15 :: Router
r15 = {nodeName = "PL15",  activeStatus = True}
r16 :: Router
r16 = {nodeName = "PL16",  activeStatus = True}
r17 :: Router
r17 = {nodeName = "PL17",  activeStatus = True}
r18 :: Router
r18 = {nodeName = "PL18",  activeStatus = False}
r19 :: Router
r19 = {nodeName = "PL19",  activeStatus = True}
r20 :: Router
r20 = {nodeName = "PL20",  activeStatus = False}
r21 :: Router
r21 = {nodeName = "PL21",  activeStatus = False}
r22 :: Router
r22 = {nodeName = "PL22",  activeStatus = True}

noNetwork :: Network
noNetwork = Termination
oneNetwork :: Network
oneNetwork = Node r1 Termination Termination
smolNetwork :: Network
smolNetwork = Node r1 (Node r2 (Node r4 Termination Termination)(Node r5 Termination Termination))(Node r3 (Node r6 Termination Termination) (Node r7 Termination Termination))
bigNetwork :: Network
bigNetwork = Node r11 (Node r4 (Node r3 (Node r1 Termination Termination) Termination) (Node r6 (Node r5 Termination Termination) Termination)) (Node r16 (Node r14 Termination (Node r15 Termination Termination)) (Node r19 (Node r18 Termination Termination) (Node r21 (Node r20 Termination Termination) (Node r22 Termination Termination))))

/*
Would you kindly write a function, that takes a Network
and a name of a Router, and returns a Bool indicating
if that Router is sucessfully connected.

A Router is successfully connected if ALL NODES
from the root to the router node have activeStatus of True.

If the Router is not found, return False.
*/
pathCheck :: Network String -> Bool
pathCheck Termination _ = False
pathCheck (Node x l r) str
|x.nodeName == str && x.activeStatus == True = True
|x.activeStatus == True  = (pathCheck l str) || (pathCheck r str)
=False
//Start = pathCheck smolNetwork "PL4" //True
//Start = pathCheck smolNetwork "PL69" //False
//Start = pathCheck bigNetwork "PL15" //True
//Start = pathCheck bigNetwork "PL22" //False
//Start = pathCheck oneNetwork "PL1" //True
//Start = pathCheck noNetwork "HelloGoodBye" //False


/*
    Create a class called Comparisons and define the operations binary operations:
     *== , != , *< , *> , *<= ,*>=
    Given two elements it compares them and gives out a boolean.
    Example: x *== y should check if the x and y are equal.
    != -> not equal
    *< -> less (smaller)
    *> -> greater (bigger)
    *<= -> less (smaller) or equal
    *>= -> greater (bigger) or equal
    )
    Make an instance for integers.
*/
class Comparisons a
where
*== :: a a ->Bool
!= :: a a -> Bool
*< :: a a -> Bool
*> :: a a -> Bool
*<= :: a a -> Bool
*>= :: a a -> Bool


instance Comparisons Int
where
*== x y = x==y
!= x y = x<>y
*< x y = x<y
*> x y = x>y
*<= x y = x<=y
*>= x y = x>=y


//Start = 3 *== 3 //True
//Start = 3!=3//False
//Start = 3*<5//True


