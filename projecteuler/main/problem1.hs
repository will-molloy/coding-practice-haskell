-- https://projecteuler.net/problem=1

numsBelow1000 = [1..999]

numsDivisibleBy3Or5 :: [Int] -> [Int]
numsDivisibleBy3Or5 list = [x | x <- list, x `mod` 3 == 0 || x `mod` 5 == 0]

-- 233168
main = print (sum (numsDivisibleBy3Or5 numsBelow1000))
