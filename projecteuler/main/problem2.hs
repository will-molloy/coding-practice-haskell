-- https://projecteuler.net/problem=2

import Data.List

fibs :: [Int]
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

evens :: [Int] -> [Int]
evens list = filter even list

lessThan :: Int -> [Int] -> [Int]
lessThan value list = takeWhile (<value) list

-- 4613732
main = print (sum (evens (lessThan 4000000 fibs)))
