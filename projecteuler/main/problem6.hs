-- https://projecteuler.net/problem=6

problem6 :: Int -> Int
problem6 n = squareOfSum n - sumOfSquares n

sumOfSquares :: Int -> Int
sumOfSquares n = sum [x^2 | x <- [1..n]]

squareOfSum :: Int -> Int
squareOfSum n = (sum [1..n])^2

main :: IO()
-- main = print $ problem6 10 -- 2640
main = print $ problem6 100 -- 25164150
