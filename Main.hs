module Main where

import Lib

main :: IO ()

doubleMe x = x + x

--findMin x y z = min (min x y) z

doubleUs v z = v*2 + z*2

doubleUs2 x y = doubleMe x + doubleMe y + x

main = putStrLn "hello world!"

doubleNumbers x = if x <= 100
                    then x*2
                  else x 

tripleNumbers x= if x > 0 && x <= 100
                    then x*3
                 else x                     

doubleSmallNumber' x = if x > 100 then x else x * 2  + 1   

doubleSmallNumber2 x = (if x > 100 then x else x * 2)  + 1         

conanO'Brien = "It's a-me, Conan O'Brien!" 

insertFive= 5:[1,2,3,4,5]

multiple13= take 24 [13,26..]

infiniteCycle = take 12 (cycle multiple13)

doubleSet xs= [x*2 | x <- [1..10], x*2 >= 12]
--functional input -- binding set -- predicate

testingONE xp= [x | x <- [50..100], x `mod` 7 == 3]

bang xt = [ if x < 10 then "BOOM" else "BANG" | x <- xt, odd x ]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

-- prediCates xs= [x | x <- [10..20], x /=13, x/=15, x/=19 ]
-- cartesianProduct with two lists [ x*y | x <- [2,5,10], y <- [8,10,11]] 
-- What if we wanted all possible products that are more than 50?
-- [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

length3 nt = sum [1 | _ <- nt]
--_ means that we don't care what we'll draw from the list anyway so instead of writing a variable name that we'll never use
-- This function replaces every element of a list with 1 and then sums that up. This means that the resulting sum will be the length of our list.
removeLower st= [ch | ch <- st, ch `elem`['A'..'Z']]
--Here's a function that takes a string and removes everything except uppercase letters from it.
nestedListOddRemover xxs= [[x| x <- xs, even x] | xs <- xxs]
-- Nested list comprehensions are also possible if you're operating on lists that contain lists. A list contains several lists of numbers. Let's remove all odd numbers without flattening the list.
-- you can write list comprehensions across several lines

-- Tuple
returnFirstTuple = fst (8,11) 
-- fst (8,11) 
returnSecondTuple= snd ("Wow", False)
-- False  

--Note: these functions operate only on pairs. They won't work on triples, 4-tuples, 5-tuples, etc. We'll go over extracting data from tuples in different ways a bit later.

combineListToTuple= zip [1,2,3,4,5] [5,5,5,5,5]  
-- [(1,5),(2,5),(3,5),(4,5),(5,5)] 
combineInfiniteList= zip [1..] ["apple", "orange", "cherry", "mango"]  
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]  

rightTriangles= [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]
-- works
rightTriangles'= [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- in order to remove (4,3,5) (3,4,5) which are same triangles we have to define b < c and a < b
rightTriangleSpecPerimetLimit xs x= [ (a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c== xs]

-- Defining recursion functions with pattern Matching
fact' :: Integer -> Integer      
fact' (-1) = error " negative argument "
fact' (0) = 1
fact' (n) = n * fact' (n-1)
-- IMPORTANT PATTERN MATCHING LIKE THIS doesn't take account of all the negative cases! only "-1"

-- REFINED PATTERN MATCHING USING CASE STATEMENT
factorial :: Integer -> Integer
factorial n = case compare n 0 of   -- COMPARE N and 0 from the choices "of" n ...
  LT -> error "Invalid Argument"   -- n less than 0 do...
  GT -> n * factorial (n-1)   --- n greater than 0 do ...
  EQ -> 1                       --- n equal to 0 do ...


-- Defining recursion function with GUIDED CONDITIONAL EQUATIONS
fact'' :: Integer -> Integer
fact'' n 
        | n < 0 = error "You have Negative Argument" -- otherwise = error " negative argument "
        | n == 0 = 1
        | n > 0 = n * fact'' (n-1)

-- Defining Fibonacci with Conditional, Guarded, and pattern matching functions
fibPosCond :: Integer -> Integer
fibPosCond n = if n < 0 then error "Negative Integer" else if n == 0 then 0 else if n == 1 || n == 2 then 1 else fibPosCond (n-1) + fibPosCond (n-2)

-- Notice that in conditiona form the result of the function definition is followed by the word "then"
-- while for Guarded statements it is followed by the symbol "="
fibPosGuard :: Integer -> Integer
fibPosGuard n 
              | n < 0 = error "Negative Argument"
              | n == 0 = 0
              | n == 1 = 1
              | n == 2 = 1
              | n > 2 = fibPosGuard (n-1) + fibPosGuard(n-2)
              
fib1 n = xs !! (n-1) where xs = 1 : 1: zipWith (+) xs (tail xs)              

fibPosPattern :: Integer -> Integer
--fibPosPattern (-1) = error "negative argument" would only work for "-1 cases"
fibPosPattern n = case compare n 0 of
        LT -> error "invalid Argument for positive Fibonacci"
        EQ -> 0
        GT -> case compare n 2 of
                LT -> 1
                EQ -> 1
                GT -> fibPosPattern (n-1) + fibPosPattern (n-2)

    
--fibPosPattern 0 = 0
--fibPosPattern 1 = 1
--fibPosPattern 2 = 1
--fibPosPattern n = fibPosPattern (n-1) + fibPosPattern (n-2)

testList= [-3]:[-5,8]:[[1,3,5,7,9],[],[4,6,8,10],[1,3,6,7,9],[],[1]]

removingOdds' xxs = [ [x | x <-xs, even x] | xs <- xxs, [] /= xs ]
-- removingOdds' on testlist gives
--[[],[8],[],[4,6,8,10],[6],[]] since the two "[]" existent gets removed from the set
removingOdds'' xxs = [ [x | x <-xs, even x] | xs <- xxs ]
-- removingOdds'' on testlist gives
--[[],[8],[],[],[4,6,8,10],[6],[],[]]  the "[]" from testList are kept as well
removingOnlyPosOdds xxs = [ [x | x <-xs, not (odd x && x > 0 )] | xs <- xxs, [] /= xs ] 

-- removingOnlyPosOdds gives
-- [[-3],[-5,8],[],[4,6,8,10],[6],[]]  the "[]" from testList are removed and only Odd positives are removed
-- Also brackets in the inner nested set loop conditions are very important
removingOnlyPosOdds' xxs = [ ys | xs <- xxs, let ys = [x | x <-xs, not (odd x && x > 0 )], not (null ys)  ]


--fibT :: Integer -> Integer
--fibT 1 = 1
--fibT 2 = 1
--fibT n = fibT (n - 1) + fibT (n - 2)
fibT n = xs !! (n - 1) where xs = 1 : 1 : zipWith (+) xs (tail xs)         