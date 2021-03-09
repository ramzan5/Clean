module CW7
import StdEnv

::Person={name::String, age::Int, height::Int}
Nikola::Person
Nikola={name="Nikola",age=19,height=194}
Ivana::Person
Ivana={name="Ivana",age=18,height=163}
Mark::Person
Mark={name="Mark",age=15,height=1170}
Michael::Person
Michael={name="Michael",age=16,height=180}

/*Given a list of Persons. Write a function which 
will decrease the height by 5, and increase the age by 2 of each Person*/

//fun2::[Person]->[Person]
fun2 [] = []
fun2 list = [{x & age = x.age + 2, height = x.height + 5 }\\x<-list]

//fun2 list = map (\x = {x & age = x.age - 2 , height = x.height - 5}) list 
Start=fun2 [Nikola,Mark,Michael]//[(Person "Nikola" 21 189),(Person "Mark" 17 1165),(Person "Michael" 18 175)]
//Start=fun2 [Ivana,Mark,Michael]//[(Person "Ivana" 20 158),(Person "Mark" 17 1165),(Person "Michael" 18 175)]
//Start=fun2 []//[]
