module Prac
import StdEnv

//f n = prod[1..n]
double x = x + x
quad x = double (double x)
//Start = quad 100.12


abs1 x
| x<0 = ~x
= x
//Start = abs1 4

fac n
| n == 0 =1
= n * fac (n-1)
//Start = fac 4

power x n
|n==0 =1
=x*power x (n-1)
//Start = power 2 4

/*Write a function that, given a grade as a Real,
//will return the respective grade as an Int.
//The grade cutoffs are as follows:
1: 0% - 50%
2: 50% - 60%
3: 61% - 70%
4: 71% - 85%
5: 86% - 100%
Return -1 for any invalid input (such as a negative grade).
*/
//myGrade :: Real -> Int
myGrade n

Start = myGrade 25.5 //1
//Start = myGrade 90.24 //5
//Start = myGrade -3.42 //-1

















