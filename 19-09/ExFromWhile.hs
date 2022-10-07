module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile x f ls = [l | l <- (drop x ls), f l]
-- fromWhile x f = filter f . drop x

fromFor :: Int -> Int -> [a] -> [a]
fromFor x y ls = [l | l <- (take y (drop x ls))]
-- fromFor x y = take y . drop x

fromTo :: Int -> Int -> [a] -> [a]
--fromTo x y ls = [l | l <- (take (y - x +1 ) (drop x ls))]
--fromTo x y = take (y-x + 1). drop x
-- from x y = drop x . take (y+1)
fromTo x y = fromFor x (y-x+1)

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat x y f ls = [l | l <- (drop x (take (y+1) ls)), f l]
-- fromFor x y f = filter f . fromTo x y
