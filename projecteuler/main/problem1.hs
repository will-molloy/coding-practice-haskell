-- https://projecteuler.net/problem=1

problem1 :: Int -> Int
problem1 n = sum (numsDivisibleBy3Or5 (numsBelow n))

numsBelow :: Int -> [Int]
numsBelow n = [1..(n-1)]

numsDivisibleBy3Or5 :: [Int] -> [Int]
numsDivisibleBy3Or5 list = [x | x <- list, x `mod` 3 == 0 || x `mod` 5 == 0]


-- main = print (problem1 10) -- 23
main = print (problem1 1000) -- 233168
