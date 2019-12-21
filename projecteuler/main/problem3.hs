-- https://projecteuler.net/problem=3

problem3 :: Int -> Int
problem3 n = head (reversePrimeFactors n)

reversePrimeFactors :: Int -> [Int]
reversePrimeFactors n = [x | x <- reverse([1..(isqrt n)]), n `mod` x == 0 && isPrime x]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = null [x | x <- [2..(isqrt n)], n `mod ` x == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral


-- main = print (problem3 13195) -- 29
main = print (problem3 600851475143) -- 6857
