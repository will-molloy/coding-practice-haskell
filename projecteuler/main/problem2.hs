-- https://projecteuler.net/problem=2

import Data.List

problem2 :: Int -> Int
problem2 n = sum (evens (lessThan n fibs))

fibs :: [Int]
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

evens :: [Int] -> [Int]
evens list = filter even list

lessThan :: Int -> [Int] -> [Int]
lessThan value list = takeWhile (<value) list


-- main = print (problem2 10) -- 10
main = print (problem2 4000000) -- 4613732
