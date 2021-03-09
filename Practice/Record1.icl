module Record1
import StdEnv



:: Date = {year :: Int, month :: String, day :: Int}
evanBDay :: Date
evanBDay = {year=2000, month="Apr", day=1}
evanBDay = {month="April",day=1,year=2000}
//Start = evanBDay.day //(Date 2000 "April" 1)
//Start = evanBDay.month //"April"
//Start = evanBDay.year
//Start = {year = 2000} //compile error
/*
:: Person = {
                name :: String,
                favoriteColors :: [String],
                birthday :: Date,
                isSingle :: Bool
            }

Evan :: Person
Evan = {birthday = evanBDay, favoriteColors = ["Red","Blue","Green"], name = "Evan", isSingle = True}

Jack :: Person
Jack = {isSingle = False,favoriteColors = ["Red","Blue"], birthday = {month = "Jan", day = 24, year = 1999}, name = "Jack"}
//Start = Jack.favoriteColors
//Start = Evan//(Person "Evan" ["Red","Blue","Green"] (Date 2000 "April" 1) True)
//Start = Evan.birthday.month //"April"
//Start = hd Evan.favoriteColors //"Red"


makeBabies :: Person Int -> [Person]
makeBabies p x = [ {p & name = p.name +++ (toString j) }\\ j<-[1..x]]

//Start = makeBabies Evan 10


//Start = [((a,b,c),a+b+c) \\ (a,b,c)<-[(1,2,3),(4,5,6),(7,8,9),(10,11,12)]]

:: Point a b = { myx::a, myy::b }

myAwesomePoint :: Point Real Int
myAwesomePoint = {myx = 3.5, myy = 4} //(Point 3.5 4.4)

myNewPoint :: Point String Bool
myNewPoint = {myx = "Ali", myy = True}
//Start = myNewPoint.myy

*/
// Given three Vectors in 2D, decide if their endpoint lie on a same line.
// Hint1: Points are on a same line, if area of triangle formed by these points is 0.
// Hint2: https://www.dummies.com/education/math/algebra/finding-the-area-of-a-triangle-using-its-coordinates/
:: Vector2 = {x :: Real, y :: Real}
getSlope :: Vector2 Vector2 -> Real
getSlope a b = (b.y - a.y)/(b.x - a.x)

collinear :: Vector2 Vector2 Vector2 -> Bool
collinear a b c = getSlope a b == getSlope a c && getSlope a b == getSlope b c

//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 0.0} // True
// Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 1.0} // False
// Start = collinear {x = 0.0, y = -1.0} {x = 2.0, y = 0.0} {x = 3.0, y = 0.0} // False



:: Person = { name :: String, gpa :: Real }

//Generator :: [String] [Int] -> [Person]
Generator names grades = [{ name = n, gpa = m } \\ n <- names & m <- grades]
//Start = Generator ["Evan", "Tringa"] [1, 4] // [(Person "Evan" 2.5), (Person "Tringa" 2.5)]
//Start = Generator ["Evan", "Tringa", "Viktoria"] [1.0, 4.0, 7.5] // [(Person "Evan" 4), (Person "Tringa" 4), (Person "Viktoria", 4)]





