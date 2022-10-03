module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "Empty list"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "Empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (x:xs) = 1 + length(xs)


sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + (sum xs)

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (reverse xs) ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs
(++) xs []= xs
(++) (y:ys) xs =  y : (ys ++ xs)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs= x : xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "No element to be the minimum"
minimum [x] = x
minimum (x:xs) = if x <= y then x else y
                where y = minimum xs

maximum :: Ord a => [a] -> a
maximum [] = error "No element to be the maximum"
maximum [x] = x
maximum (x:xs) = if x <= y then y else x
                where y = maximum xs

take :: Int -> [a] -> [a]
take 0 xs = []
take n (y:ys) = y : (take (n-1) ys)

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (y:ys) = (drop (n-1) ys)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) = if f x then x : takeWhile f xs 
                     else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) = if f x then dropWhile f xs 
                     else (x:xs)
tails :: [a] ->[[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : (tails xs)
-- tails "hello" = ["hello","ello","llo","lo","o",""]
init :: [a] -> [a]
init [x] = []
init (x:xs) = x:(init xs)

inits :: [a] ->[[a]]
inits [] = [[]]
inits (x:xs) = [] : (L.map (x:) (inits xs)) 
-- inits "hello" = ["","h","he","hel","hell","hello"]

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome xs = xs == (reverse xs)

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}