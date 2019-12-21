-- https://projecteuler.net/problem=3

problem3 :: Int -> Int
problem3 n = head (reversePrimeFactors n)

reversePrimeFactors :: Int -> [Int]
reversePrimeFactors n = [x | x <- reverse([1..(isqrt n)]), n `mod` x == 0 && isPrime x]

isPrime :: Int -> Bool
isPrime n = not (exists divides n [2..(isqrt n)])
    where divides a b = a `mod` b == 0 

exists :: (Int -> Int -> Bool) -> Int -> [Int] -> Bool
exists p n [] = False
exists p n (head:tail) = p n head || exists p n tail

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO()
-- main = print (problem3 13195) -- 29
main = print (problem3 600851475143) -- 6857
