-- https://projecteuler.net/problem=7

problem7 :: Int -> Int
problem7 n = primes !! (n - 1)

primes :: [Int]
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

main :: IO()
-- main = print (problem7 6) -- 13
main = print (problem7 10001) -- 104743
