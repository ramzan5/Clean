module Record
import StdEnv

/*
/*
Records are a way to hold different types of data in an organized fashion. ?(´• ? •`?)
*/

myBirthday :: (Int, String, Int)
myBirthday = (2000, "April", 01)
//Start = thd3 myBirthday //"April"
yourBirthday :: (String, Int, Int)
yourBirthday = ("April",01,2000)
//Start = snd3 yourBirthday //1

:: Date = {
            year :: Int,
            month :: Month,
            day :: Int
          }
evanBDay :: Date
evanBDay = {year=2000, month=Apr, day=1}
evanBDay = {month="April",day=1,year=2000}
//Start = evanBDay //(Date 2000 "April" 1)
//Start = evanBDay.month //"April"
//Start = evanBDay.year
//Start = {year = 2000} //compile error

:: Person = {
                name :: String,
                favoriteColors :: [String],
                birthday :: Date,
                isSingle :: Bool
            }

Evan :: Person
Evan = {birthday = evanBDay, favoriteColors = ["Red","Blue","Green"], name = "Evan", isSingle = True}

Jack :: Person
Jack = {isSingle = False,favoriteColors = ["Red","Blue"], birthday = {month = Jan, day = 24, year = 1999}, name = "Jack"}
//Start = Jack.favoriteColors
//Start = Evan //(Person "Evan" ["Red","Blue","Green"] (Date 2000 "April" 1) True)
//Start = Evan.birthday.month //"April"
//Start = hd Evan.favoriteColors //"Red"

:: Point a = {x::a,y::a,z::a}

p1 :: (Point Int)
p1 = {x=1,y=2,z=3}
//Start = p1 //(Point 1 2 3)
p2 :: (Point Real)
p2 = {x=1.234, y=2.345, z=3.456}
//Start = p2 //(Point 1.234 2.345 3.456)
p3 = {x=True,y=False,z=True}
//Start = p3.y

//:: WeirdRecord a b c d e f = {x::(a,b),y::[c],z::e, s::a, r::Bool}
//wr = {x=("Evan",41.343),y=[3,5,6,3,5],z="Hello",s="Bye",r=True}

//Start = wr //(WeirdRecord ("Evan",41.343) [3,5,6,3,5] "Hello" "Bye" True)
// :: Person = {
//                 name :: String,
//                 favoriteColors :: [String],
//                 birthday :: Date,
//                 isSingle :: Bool
//             }
// :: Date = {
//             year :: Int,
//             month :: String,
//             day :: Int
//           }
// getBirthMonth :: Person -> String
// //getBirthMonth p = p.birthday.month
// //getBirthMonth {name = n, favoriteColors = f, birthday = b, isSingle = s} = b.month
// getBirthMonth p=:{birthday = {month = m}}
// | p.isSingle = m
// = "blah"

//Start = getBirthMonth Evan //"April"

updateSingleStatus :: Person -> Person
//updateSingleStatus p = {isSingle = not p.isSingle, name = p.name, favoriteColors = p.favoriteColors, birthday = p.birthday}
//updateSingleStatus p = {p & isSingle = not p.isSingle }
updateSingleStatus p=:{isSingle = s} = {p& isSingle = not s}
//Evan = {birthday = evanBDay, favoriteColors = ["Red","Blue","Green"], name = "Evan", isSingle = True}
//p = {birthday = evanBDay, favoriteColors = ["Red","Blue","Green"], name = "Evan", isSingle = True}
//s = True
//{p& isSingle = not s} = {birthday = evanBDay, favoriteColors = ["Red","Blue","Green"], name = "Evan", isSingle = False}
//Start = updateSingleStatus Evan //(Person "Evan" ["Red","Blue","Green"] (Date 2000 "April" 1) False)

updateColors :: Person -> Person
updateColors p=:{isSingle = s, favoriteColors = f}
// | p.isSingle = {p & isSingle = not p.isSingle, favoriteColors = p.favoriteColors ++ ["White"]}
// = {p& isSingle = not p.isSingle, favoriteColors = []}
| s = {p & isSingle = not s, favoriteColors = f ++ ["White"]}
= {p& isSingle = not s, favoriteColors = []}

//Start = updateColors (updateSingleStatus Evan)

:: Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

//Start = Evan.birthday.month == Apr //Overloading error [records.icl,95,Start]: "==" no instance available of type Month

instance == Person
where
//    (==) p1 p2 = p1.name == p2.name && p1.isSingle == p2.isSingle
    (==) {name=n1, birthday=b1} {name=n2, birthday=b2} = n1 == n2 && b1==b2
//operator overloading <- THIS IS NOT WHAT IT IS CALLED IN FUNCTIONAL 
instance == Date where (==) {day=d1, year = y1, month = m1} {day=d2, year = y2, month=m2} = d1==d2 && y1==y2 && m1==m2

instance == Month
where
    (==) Jan Jan = True
    (==) Feb Feb = True
    (==) Mar Mar = True
    (==) Apr Apr = True
    (==) May May = True
    (==) _ _ = False

//Start = Evan == Jack 

instance < Person 
where
    (<) {birthday={year=y1}} {birthday={year=y2}} = y1 < y2
// if you define an instance for '<', the 'Ord' class will automatically define and create for you
//the >, >=, <= instances
// if you define an instance for '==', the 'Eq' class will automatically define and create for you
//the <> instance

//Start = Evan < Jack

:: Gender = Male | Female | AttackHelicopter

listPeople = [Evan, Jack]
//Start = isMember Evan listPeople
//Start = sort listPeople

defaultPersonRecord::Person
defaultPersonRecord = {name="",favoriteColors=[],isSingle=True,birthday={year=0,day=0,month=Jan}}

makeBabies :: Person Int -> [Person]
makeBabies p=:{name=n} x = [ {p& name = n +++ (toString j) }\\ j<-[1..x]]

//Start = makeBabies Evan 10
//[(Person "Evan1" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan2" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan3" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan4" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan5" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan6" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan7" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan8" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan9" ["Red","Blue","Green"] (Date 2000 Apr 1) True),(Person "Evan10" ["Red","Blue","Green"] (Date 2000 Apr 1) True)]

func :: (Int,Int,Int) -> ((Int,Int,Int),Int)
func t=:(a,b,c) = (t,a+b+c)

//Start = func (1,2,3) //((1,2,3),6)
*/




/*
Records are a way to hold different types of data in an organized fashion. ?(´• ? •`?)


myBirthdayTuple :: (Int, String, Int)
myBirthdayTuple = (2000,"April",01)

yourBirthdayTuple :: (String, Int, Int)
yourBirthdayTuple = ("April",01,2000)
//Start = snd3 myBirthdayTuple
//Start = snd3 yourBirthdayTuple

:: Date = {
            year :: Int,
            month :: Month,
            day :: Int
          }
evanBirthday :: Date
evanBirthday = { year = 2000, month = Apr, day = 1 }
//evanBirthday = {month = "April", day = 1, year = 2000}
//Start = evanBirthday //(Date 2000 "April" 1)
//Start = evanBirthday.month //"April"

:: Person = {
                name :: String,
                favoriteColor :: [String],
                birthday :: Date,
                isSingle :: Bool
            }
Evan :: Person
Evan = { isSingle = True, favoriteColor = ["Red","Blue","Green"], birthday = evanBirthday, name = "Evan" }

//Start = Evan //(Person "Evan" "Red" (Date 2000 "April" 1) True)

:: Point a = { x::a, y::a, z::a }
p1 :: (Point Int)
p1 = {x=0, y=2, z=3}
p2 :: (Point Real)
p2 = {x=1.234, y=2.456, z=3.1456}

// p3 = { x=True, y=False, z=True}

//Start = p1 //(Point 0 2 3)
//Start = p2 //(Point 1.234 2.456 3.1456)
//Start = p3 //(Point True False True)

//:: Random a b c = { x :: a, y :: b, z :: c, extra :: String, w :: (a,b)}
//Start = {x=4, y=True, z=754848.48485, extra = "Hello", w=(5,False)}

getSingleStatus :: Person -> Bool
//getSingleStatus p = p.isSingle
//getSingleStatus {name = n, favoriteColor = c, birthday = b, isSingle = s} = s
getSingleStatus p=:{isSingle = s} = s
getcolor p =:{favoriteColor = s} = s
//Start = getSingleStatus Evan //True
//Start = getcolor Evan
updateSingleStatus :: Person -> Person
//updateSingleStatus p = {name = p.name, favoriteColor = p.favoriteColor, birthday = p.birthday, isSingle = not p.isSingle}
//updateSingleStatus p = {p & isSingle = not p.isSingle}
updateSingleStatus p=:{isSingle = s} = {p & isSingle = not s}

//Start = updateSingleStatus Evan //(Person "Evan" "Red" (Date 2000 "April" 1) False)

newProfile :: Person -> Person
newProfile p=:{isSingle = s, favoriteColor = f}
| s = { p & isSingle = not s, favoriteColor = f ++ ["white"]}
= {p & favoriteColor = []}

//Start = newProfile Evan //(Person "Evan" ["Red","Blue","Green","white"] (Date 2000 Apr 1) False)
//Start = newProfile (updateSingleStatus Evan) //(Person "Evan" [] (Date 2000 "April" 1) False)

:: Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec 


instance == Month
where
    (==) Jan Jan = True
    (==) Feb Feb = True
    (==) Mar Mar = True
    (==) Apr Apr = True
    (==) May May = True
    (==) _ _ = False 


//Start = Evan.birthday.month //Apr
//Start = Evan.birthday.month == Jul //Overloading error [records.icl,78,Start]: "==" no instance available of type Month

makeDate :: (Int, Month, Int) -> Date
makeDate (y,m,d)
| y <= 0 || d <= 0 = abort "Invalid input\n"
= {year = y, month = m, day = d}

makePerson :: (String,Int,Month,Int) -> Person
makePerson (n,y,m,d)
| n <> "" = {name = n, favoriteColor = [], isSingle = True, birthday = makeDate (y,m,d)}
= abort "Empty Name input\n"

//Start = makeDate (2000, Apr, 1)
//Start = makeDate (2000, Apr, -4)
//Start = makeDate (2000, Blahblah, 1)
//Start = makePerson ("Evan",2000,Apr,1) //(Person "Evan" [] (Date 2000 Apr 1) True)
//Start = makePerson ("Evan",2000,Apr,-1)

//[ {field1 = a+b, field2 = a-b} \\ {field1 = a, field2 = b}<-listOfRecords ]
//map (\p = {p & field1 = newvalue } ) listofRecords

*/
// add :: Int Int -> Int 

// Start = fst("hossam", 19) //"hossam"
// Start = fst(19, "hossam") //19

// Start = fst3 (2020, 04, 24) //2020
// Start = fst3 (24, 04, 2020) //24

// Records

          
// Start = { day = 24, month = "April", year = 2020 } //(Date 2020 4 24)

// :: Point a b = { myx::a, myy::b }

// myAwesomePoint :: Point Real Int
// myAwesomePoint = {myx = 3.5, myy = 4} //(Point 3.5 4.4)

// Start = myAwesomePoint

//:: Person = {
    //            name :: String,
   //             age :: Int,
  //               birthday :: Date,
 //                martial_status :: Bool
//            }

// Hossam :: Person
// Hossam = { name = "Hossam", age = 20, birthday = hossamBD, martial_status = False } 

// Start =  Hossam //(Person "Hossam" 19 (Date 2000 "May" 8) False)

// Give me the age of hossam
// Start = Hossam.age //19

// Give me the age of Person
// getAge :: Person -> Int
// getAge { name = n, age = a, birthday = b, martial_status = m } = a
// getAge p = p.age

// Start = getAge Hossam //19

//martialStatusUpdator :: Person -> Person
///martialStatusUpdator p
//| p.age < 20 && not p.martial_status = p
//= {p&martial_status = True, age = 600}

//Start = martialStatusUpdator Evan //(Person "Hossam" 20 (Date 2000 "May" 8) True)


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

// Given a list of distinct name and a list of grade. 2 lists have the same length
// Generate a list of Person corresponding to the name list
// the grades of all Person should be the average of the 2nd list.
// Hint: The record Person should contain at least `name` and `average grade`

myAvg :: [Int] -> Real
myAvg list = avg [toReal g \\ g <- list]

:: Person = { name :: String, gpa :: Real }

Generator :: [String] [Int] -> [Person]
Generator names grades = [{ name = n, gpa = myAvg grades } \\ n <- names]

//Start = Generator ["Evan", "Tringa"] [1, 4] // [(Person "Evan" 2.5), (Person "Tringa" 2.5)]
// Start = Generator ["Evan", "Tringa", "Viktoria"] [1, 4, 7] // [(Person "Evan" 4), (Person "Tringa" 4), (Person "Viktoria", 4)]


// Algebric data type

:: Month = Jan | Feb | Mar | Apr | May | Jun | July | Aug | Sep | Oct | Nov | Dec

instance == Month
where
    (==) Jan Jan = True
    (==) Feb Feb = True
    (==) Mar Mar = True
    (==) Apr Apr = True
    (==) _ _ = False

//Start = Jan













