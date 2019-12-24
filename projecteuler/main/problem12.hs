-- https://projecteuler.net/problem=12

import Data.List (find)

problem12 :: Int -> Maybe Int
problem12 n = find ((>n) . numDivisors) triangleNums

triangleNums :: [Int]
triangleNums = scanl1 (+) [1..]

numDivisors :: Int -> Int
numDivisors n = 2 * (length $ filter (\x -> n `mod` x == 0) [1..isqrt(n)])

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO()
main = do
    print $ problem12 5 -- 28
    print $ problem12 500 -- 76576500
