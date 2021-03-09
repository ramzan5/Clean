module Task4
import StdEnv

/*
Given a list of numbers increase all the elemnts by one using higher order function
*/
increment:: [Int] -> [Int]

increment [] = []
increment list = map inc list


//Start = increment [1..5] //[2,3,4,5,6]
//Start = increment [-5..5] //[-4,-3,-2,-1,0,1,2,3,4,5,6]
//Start = increment []//[]

