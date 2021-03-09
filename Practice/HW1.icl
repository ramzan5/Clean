module HW1
import StdEnv

/*
Write a function that, given a grade as a Real,
will return the respective grade as an Int.
The grade cutoffs are as follows:
1: 0% - 50%
2: 50% - 60%
3: 61% - 70%
4: 71% - 85%
5: 86% - 100%
Return -1 for any invalid input (such as a negative grade).
*/
//
//Start = myGrade 25.5 //1
//Start = myGrade 90.24 //5
//Start = myGrade -3.42 //-1
 myGrade :: Real -> Int
myGrade x
| x > 0.0 && x <= 50.0 = 1
| x > 50.0 && x <= 60.0 = 2
| x > 60.0 && x <= 70.0 = 3
| x > 70.0 && x <= 85.0 = 4
| x > 85.0 && x <= 100.0 = 5
| x < 0.0 = -1
Start = myGrade -3.42
*/
/*
Write a function that will take a number (Int)
and return the respective month (String).
For example: input of 1 should output January.
Return "Invalid month" (or something similar)
for any invalid input (such as negative numbers or
numbers bigger than 12.)*/

//monthString :: Int -> String
//Start = monthString 4 //"April"
//Start = monthString 10 //"October"
//Start = monthString -1 //"Not a valid month"
//Start = monthString 42 //"Not a valid month"
/*
monthString x
| x == 1 = "January"
| x == 2 = "February"
| x == 3 = "March"
| x == 4 = "April"
| x == 5 = "May"
| x == 6 = "June"
| x == 7 = "July"
| x == 8 = "August"
| x == 9 = "September"
| x == 10 = "October"
| x == 11 = "November"
| x == 12 = "December"
| x =< 0 = "Not a valid month"
| x > 12 = "Not a valid month"
Start = monthString 42
*/
/*
Write a function that will calculate the total
of your dining bill after including gratuity.
Take the subtotal as a Real and the gratuity percentage
as a Real.
For example:
If the subtotal is 10.00, and gratuity is 0.15, then
the total is 10.00 + (0.15 * 10.00) = 11.50
*/
//myBill :: Real Real -> Real
//Start = myBill 10.00 0.15 //11.50
//Start = myBill 9001.00 0.08 //9721.08

myBill x y = x + (y * x)
  

Start = myBill 9001.00 0.08





  

