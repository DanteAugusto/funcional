module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where
-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat 0 0) = "NaN"
    show (Rat _ 0) = "Infinity"
    show (Rat x y) = show x++"/"++ show y

instance Eq Rat where
    (==)(Rat x y) (Rat c d)
        | x == c && y == d = True
        | x > c && y > d = if (div x c == (div y d)) && (mod x c == 0) && (mod y d == 0) then True else False
        | c > x && d > y = if (div c x == (div d y)) && (mod c x == 0) && (mod d y == 0) then True else False
        | otherwise = False

instance Num Rat where
    (+) :: Rat -> Rat -> Rat
    (+) (Rat x y) (Rat c d)= (Rat (x*d + c*y) (y*d))
    (*) :: Rat -> Rat -> Rat
    (*) (Rat x y) (Rat c d)= (Rat (x*c) (y*d))
    negate :: Rat -> Rat
    negate (Rat x y)= (Rat (x*(-1)) y)
    abs :: Rat -> Rat
    abs (Rat x y)= if signum (Rat x y) == 1 then Rat x y else Rat (x*(-1)) y
    --signum :: Rat -> Integer
    signum (Rat x y)
        | y == 0 && x == 0 = Rat 0 0
        | y == 0 = if x > 0 then 1 else -1
        | x == 0 = 0
        | otherwise = if x*y > 0 then 1 else -1
    fromInteger :: Integer -> Rat
    fromInteger x = (Rat x 1)

instance Ord Rat where
    compare (Rat x y) (Rat c d) = if (x*d) <= (c*y) then if (x*d) == (c*y) then EQ else LT else GT

rat :: Integer -> Integer -> Rat
rat x y = (Rat x y)

(//) :: Rat -> Rat -> Rat
(//) (Rat x y) (Rat c d) = (Rat x y) (Rat d c)

denominator :: Rat -> Integer
denominator (Rat _ x ) = x

numerator :: Rat -> Integer
numerator (Rat x _ ) = x

