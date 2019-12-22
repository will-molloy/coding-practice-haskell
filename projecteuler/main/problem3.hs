-- https://projecteuler.net/problem=3

problem3 :: Int -> Int
problem3 = head . reversePrimeFactors

reversePrimeFactors :: Int -> [Int]
reversePrimeFactors n = [x | x <- reverse([1..(isqrt n)]), n `mod` x == 0 && isPrime x]

isPrime :: Int -> Bool
isPrime n = not $ any divides [2..(isqrt n)]
    where divides a = n `mod` a == 0

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO()
-- main = print $ problem3 13195 -- 29
main = print $ problem3 600851475143 -- 6857
