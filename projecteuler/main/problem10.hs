-- https://projecteuler.net/problem=10

problem10 :: Int -> Int
problem10 n = sum $ takeWhile (<n) primes

-- https://reddit.com/r/haskell/comments/35vc31/the_real_way_to_generate_a_list_of_primes_in/
primes :: [Int]
primes = 2 : 3 : 5 : primes'
    where
        primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
        isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n

main :: IO()
-- main = print $ problem10 10 -- 17
main = print $ problem10 2000000 -- 142913828922
