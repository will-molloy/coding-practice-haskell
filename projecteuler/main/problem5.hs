-- https://projecteuler.net/problem=5

import Data.List (find)

problem5 :: Int -> Maybe Int
problem5 n = find (divisibleByAll [1..n]) [1..]

divisibleByAll :: [Int] -> Int -> Bool
divisibleByAll list n = all divides list
    where divides a = n `mod` a == 0
    
main :: IO()
main = print $ problem5 10 -- 2520
-- main = print $ problem5 20 -- 232792560
