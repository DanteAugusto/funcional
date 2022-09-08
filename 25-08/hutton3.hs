-- 1. What are the types of the following values?
-- [’a’,’b’,’c’] :: [Char]
-- (’a’,’b’,’c’) :: (Char,Char,Char)
-- [(False,’O’),(True,’1’)] :: [(Bool,Char)]
-- ([False,True],[’0’,’1’]) :: ([Bool],[Char])
-- [tail, init, reverse] :: [[a] -> [a]]

--2. Write down definitions that have the following types; it does not matter what
--   the definitions actually do as long as they are type correct.
-- bools :: [Bool]. bools = [true,false,false]
-- nums :: [[Int]]. nums = [[1],[1,2][1,2,3]]
-- add :: Int -> Int -> Int -> Int. add x y z = x + y + z
-- copy :: a -> (a,a). copy x = (x,x)
-- apply :: (a -> b) -> a -> b. apply f a = f a

-- 3. What are the types of the following functions?
-- second xs = head (tail xs). second :: [a] -> a
-- swap (x,y) = (y,x). swap :: (a,b) -> (b,a)
-- pair x y = (x,y). pair :: a -> b -> (a,b)
-- double x = x*2. double:: Num a => a -> a
-- palindrome xs = reverse xs == xs. palindrome :: Eq a => [a] -> Bool
-- twice f x = f (f x). twice :: (a -> a) -> a ->a
-- Hint: take care to include the necessary class constraints in the types if the
-- functions are defined using overloaded operators.

-- 4. All tests runned.

-- 5. Why is it not feasible in general for function types to be instances of the Eq
-- class? When is it feasible? Hint: two functions of the same type are equal if
-- they always return equal results for equal arguments. It is not feasible because
-- a function has possibly infinite combinations of one element from the domain and one 
-- from the codomain. Because of that, we cant compare all of that combinations between two
-- functions, wich go against the definition of Eq classe. Although, it is feasible to compare
-- two functions if both of them have a finite amount of combinations.