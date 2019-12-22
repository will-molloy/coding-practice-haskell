-- https://projecteuler.net/problem=7

problem7 :: Int -> Int
problem7 n = primes !! (n - 1)

-- https://reddit.com/r/haskell/comments/35vc31/the_real_way_to_generate_a_list_of_primes_in/
primes :: [Int]
primes = 2 : 3 : 5 : primes'
    where
        primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
        isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n

main :: IO()
-- main = print $ problem7 6 -- 13
main = print $ problem7 10001 -- 104743
