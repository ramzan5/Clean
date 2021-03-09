module Task5
import StdEnv

/*Given a list of Int.
Write a function which will calculate the sum of the numbers up to the first number greater than 10*/
//sumTillGreater::[Int]->Int

sumTillGreater list = foldl (+) 0 (takeWhile((>=)10) list)

//Start=sumTillGreater [1,4,10,12]//15
//Start=sumTillGreater [1,2,3,4,5,6,7]//28
//Start=sumTillGreater []//0
//Start=sumTillGreater [10,11]//10

Start = sort [2,3,,5,23,6,84,3,2]