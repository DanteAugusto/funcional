module Mian where
    -- 1.
    halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
    --2.
    thirda xs |l < 3 = error "No third element" | otherwise= head (tail (tail xs)) where l = length xs
    thirdb xs |l < 3 = error "No third element" | otherwise = xs !! 2 where l = length xs
    thirdc xs@(_:_:x:_) |l < 3 = error "No third element" | otherwise = x where l = length xs
    --3.
    safetaila xs = if (length xs) == 0 then xs else tail xs
    safetailb xs | l == 0 = [] | otherwise = tail xs where l = length xs
    safetailc [] = []
    safetailc xs = tail xs
    -- 4.
    -- (||) :: Bool -> Bool -> Bool
    -- True || True = True
    -- True || False = True
    -- False || True = True
    -- False || False = False
    -- Other way:
    -- False || False = False
    -- _ || _ = True
    -- 5.
    -- if a then if b then true else false else false
    -- 6.
    -- if a then b else false
    -- 7.
    -- \x -> (\y -> (\z -> x * y * z))
    -- 8.
    luhnDouble x | x > 9 = error "número inválido" | x < 5 = x*2 | otherwise = x*2 - 9
    luhn a b c d = ((luhnDouble a) + (b) + (luhnDouble c) + (d)) `mod` 10 == 0
    
