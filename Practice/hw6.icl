module hw6
import StdEnv

/*
Given a list of pairs of name of the person and his/her favourite food.
Make function which returns list of pairs of food and a list of people who likes it.
Note : order doesn't matter
*/
//favFood list = [(y,[x,a])\\(x,y)<-list,(a,b)<-list|y==b && x <> a]
favFood [] = []
favFood [(a,b):c] = [(b,[a:[x\\(x,y)<-c |y==b]])] ++ (favFood [(m,n)\\(m,n)<-c|n<>b])


//Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "apple")] // [("apple", ["Zuka", "Ahmed"]),("orange",["Beka"]),("pineapple",["Emad"])]
//Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "pineapple")] // [("apple", ["Zuka"]),("orange",["Beka"]),("pineapple",["Emad","Ahmed"])]

/*
Having a list of tuples, each tuple represent a person in that form (name, age, gender)
Write a function to produce a list of two elements. the older man's name and the older woman's name
i.e : [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] -> ["Hossam", "Nani"]
Note : You can assume that the input for the gender will be "male", "female".
*/



findYounger list = [x\\(x,y,z)<-list |y == malemin] ++ [x\\(x,y,z)<-list | y==femalemin]
where
     malemin= hd(sort[y\\(x,y,z)<-list | z == "male"])
     femalemin= hd(sort[y\\(x,y,z)<-list | z == "female"])
//Start = findYounger [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] // ["Hossam", "Nani"]
//Start = findYounger [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")] // ["Evan", "Tringa"]
// Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]

/*
Decide if a number is triangular number and write the count of levels of triangle. 
Triangular number is a number that can form a triangle.
The output should be in a tuple.
Note : if it is false the count should be -1. 
examples:
1       3         6          10          15
                                          *
                               *          * *
                  *            * *        * * *
        *         * *          * * *      * * * *
*       * *       * * *        * * * *    * * * * *

Note : 0 is not a triangular number
*/

tri n =  (n * (n+1))/2

triangular::Int -> (Bool,Int)
triangular n = (isMember n list, len)
where
      list = (takeWhile((>=)n)[tri x\\x<-[1..]])
      len = length list
isTringularNum n 
|x == False = (False,-1)
= (x,y)
where
     (x,y) = triangular n

//Start = triangular 
//Start = isTringularNum -1 // (False,-1)
// Start = isTringularNum 0 // (False,-1)
// Start = isTringularNum 1 // (True,1)
//Start = isTringularNum 5 // (False,-1)
//Start = isTringularNum 10 // (True,4)
//Start = isTringularNum 666 // (True,36)


















