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
snoc x [] = [x]
snoc y (x:xs)= x : (snoc y xs)

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
take _ [] = []
take 0 _ = []
take n (y:ys) = if n > 0 then y : (take (n-1) ys) else []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 ys = ys
drop n (y:ys) = if n > 0 then (drop (n-1) ys) else (y:ys)

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
inits (xs) = xs : (inits (init xs)) 
-- inits "hello" = ["","h","he","hel","hell","hello"]

subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences (x:xs) = [x]: (map (x:) (subsequences xs)) ++ (subsequences xs)


any:: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = if f x then True else any f xs

all:: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = if f x then all f xs else False

and :: [Bool] -> Bool
and [] = True
and (x:xs) = if x then and xs else x

or :: [Bool] -> Bool
or [] = False
or (x:xs) = if x then True else or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any ( == x) 

-- elem': same as elem but elementary definition
elem' :: Eq a => a -> [a] -> Bool
elem' _  [] = False
elem' y  (x:xs) = if x==y then True else elem' y xs
-- (without using other functions except (==))

(!!) :: [a] -> Int -> a
(!!) [] _ = error "index too large"
(!!) (x:xs) 0 = x
(!!) (x:xs) n = if n < 0 then error "negative index" else (!!) xs (n-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if f x  then x:(filter f xs) else filter f xs


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x:(map f xs)

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate x y | x >= 1 = y : replicate (x-1) y | otherwise = []

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = if x == y then isPrefixOf xs ys else False 

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (xs) (y : ys) = if xsz > ysz then False else if xs == (take xsz (y:ys)) then True else isInfixOf xs (ys)
                        where  
                            xsz = length xs
                            ysz = length (y : ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (xs) (y : ys) = if xsz > ysz then False else if xs == (drop (ysz-xsz) (y:ys)) then True else False
                        where  
                            xsz = length xs
                            ysz = length (y : ys)


zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y): (zip xs ys)
zip _ _ = []
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y: (zipWith f xs ys)
zipWith f _ _ = []

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate (xs) (y:ys) = y++xs++ (intercalate xs ys)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:(filter (/=x) (nub xs))


--splitAt :: Int -> [a] -> ([a],[a])
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- n can be bigger then length of xs or can be negative too

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([],[])
break f xs = (takeUntil f xs, dropUntil f xs)
            where
                takeUntil _ [] = []
                takeUntil f (x:xs) = if f x then [] else x: takeUntil f xs

                dropUntil _ [] = []
                dropUntil f (x:xs) = if f x then (x:xs) else dropUntil f xs

lines :: String -> [String]
lines xs = [xs]

words :: String -> [String]
words [] = []
words (xs) = if (dropWhile (/= ' ') xs) == [] then takeWhile (/= ' ') xs : words [] 
            else takeWhile (/= ' ') xs : words (tail(dropWhile (/= ' ') xs))

unlines :: [String] -> String
unines [] = []
unlines (x:xs) = x ++ "\n" ++ unlines xs

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