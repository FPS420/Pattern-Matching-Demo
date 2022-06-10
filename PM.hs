module PM where

    -- Simple Pattern Matching Demo --

-- Pattern Matching with Integers
-- Implementation of <fibonacci n> to calculate the Fibonacci Number
-- of n with Pattern Matching

fibonacci:: Integer -> Integer

-- if the call of fibonacci n is equal to fibonacci 0 then Haskells Compiler
-- is matching n with the specified Pattern 0

fibonacci 0 = 0

-- if the call of fibonacci n is equal to fibonacci 1 then Haskells Compiler
-- is matching n with the specified Pattern 1

fibonacci 1 = 1

-- if no Pattern is matched, then the Pattern of n is bounded on x,
-- and the result is a recusivly call of fibonacci (x-1) + fibonacci (x-2)

fibonacci x = fibonacci (x-1) + fibonacci (x-2) 


-- Pattern Matching with Lists
-- Implementation of <reverseList [a]> to revesere a given list 

reverseList:: [a] -> [a]

-- in Haskell the pattern of an empty list is [],
-- if the calls parameter of reveserList [a] a empty list,
-- then the pattern of the parameter is matched with the pattern []

reverseList [] = []

-- the other pattern of an list in Haskell, is the not empty list
-- a not empty list has the pattern of (head:tail) and this mean
-- that a not empty list includes at least one item head 
-- and the rest (= tail) of the list in form of an list without the head item
-- so head is an item of <type a> and tail is a list of <type [a]>
-- if the calls parameter of revesereList not an empty list revesereList will
-- call recursivly at least the tail has the pattern of an empty list 
-- and the first pattern of reverseList is matched and return [] instead of an recusivly call

reverseList (head:tail) = reverseList tail ++ [head]

-- Pattern Matching with Data types
-- definition of <type person>
-- a person can be a Student or an employee
-- a student has a name and a list of tuples that contains the evaluation of an education event
-- a Employee has a name and a list of education events

data Person0 = Student0 Name0 [ (Fach0, Note0) ] | Employee Name0 [Fach0] deriving Show
type Name0 = String
type Fach0 = String
type Note0 = Double

-- average calculates the average evaluation of an Student

average'':: Person0 -> Double -> Double -> Double

-- avarage first parameter is a Person, we know that a Person can be an Student or an Employee 
-- to calculate an avarage evaluation of a Student, we have to check the pattern of the
-- parameter matched the pattern of a student 
-- average'' (Student name list) = 0
-- the next step we need is to match the pattern of the placeholder list,
-- with the tuples of an student, to the patterns of an list
-- we know a list can have Patterns: empty or not empty
-- first we prove for the pattern of an empty list []
-- if the following pattern (the pattern of an empty list) matched we want to stop the recursion
-- and returning the calculatated avarage evaluation
-- in akk we have the sum of all evaluations, in n we have the number of education events.
-- the average is akk/n

average'' (Student0 name []) n akk = akk / n

-- if the pattern, of the list of tuples, of the student, isnt empty
-- we have the pattern of (x:xs), a not empty list (see: Pattern Matching with Lists)
-- average'' (Student name (x:xs)) 
-- every x represents an item of the list, an item of an list is a tuple of (Fach,Note)
-- so we can again match the pattern of an item. x = (fach,note)
-- we have matched the Pattern of an student, the list and the pattern of an item in this list.
-- now we have access to nearby everything to calculate the average evalutation
-- the only thing we dont know is the number of items in this list.
-- the simples way to check the number of items is to increment an counter by going threw the list
-- so we add an extra parameter n to increment every time when we recursivly calling avarage'',
-- to get the number of items in the list
-- we also add an akkumulator to calculate the sum of evaluations in the akkumulator

average'' (Student0 name ((fach,note):xs)) n akk = average'' (Student0 name xs) (n+1) (akk + note)

-- the last thing we have to think about is what is going on if the person isnt an Student?
-- at this moment when average'' is calling with an Employee and
-- non-exhausting pattern error will show up
-- so we have to match the Employee also and return an default value like 0.0 or -1.0

average'' (Employee name list) n akk = -1.0

-- one last thing we can now do is to define an pattern that will be machted
-- when a person is not an Student or Employee. Its called default pattern
-- maybe in the future development of person we add an extra constructor like graduate
-- a graduate is a person so average'' will look for a pattern that matched this specific pattern
-- but cant find them and the result is again an
-- non-exhausting pattern error. so we implement the default pattern for
-- every person that is not an Student or Employee

average'' _ _ _ = -1.0 

-- now we can call average'' with three Parameters. But two of them are just for
-- the calculation and not recomend for the caller
-- so we can make average'' local and define an global function
-- callAverage with only one parameter, the person

callAverage:: Person0 -> Double
callAverage person = average'' person 0 0
