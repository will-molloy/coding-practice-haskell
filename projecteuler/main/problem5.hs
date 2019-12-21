-- https://projecteuler.net/problem=5

problem5 :: Int -> Int
problem5 n = head [x | x <- [1..], divisibleByAll x [1..n]]

divisibleByAll :: Int -> [Int] -> Bool
divisibleByAll n list = forAll divides n list
    where divides a b = a `mod` b == 0
    
forAll :: (Int -> Int -> Bool) -> Int -> [Int] -> Bool
forAll p n [] = True
forAll p n (head:tail) = p n head && forAll p n tail

main :: IO()
main = print (problem5 10) -- 2520
-- main = print (problem5 20) -- 232792560
