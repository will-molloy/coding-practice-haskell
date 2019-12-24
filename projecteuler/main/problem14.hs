-- https://projecteuler.net/problem=14

import Data.List
import Data.Ord

problem14 :: Int -> Int
problem14 n = maximumBy (comparing (length . collatzSequence)) [1..(n-1)]

collatzSequence :: Int -> [Int]
collatzSequence seed = takeWhile (>1) (iterate next seed) -- omits last '1' but doesn't matter

next :: Int -> Int
next n | even n = n `div` 2
       | otherwise = n * 3 + 1

main :: IO()
main = do
    print $ problem14 100000 -- 77031
    print $ problem14 1000000 -- 837799
