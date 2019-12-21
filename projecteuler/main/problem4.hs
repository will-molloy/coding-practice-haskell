-- https://projecteuler.net/problem=4

problem4 :: Int -> Int
problem4 n = maximum (palindromes n)

palindromes :: Int -> [Int]
palindromes n = [(x*y) | x <- list, y <- list, isPalindrome (x*y)]
    where list = [(10^(n-1))..((10^n)-1)]

isPalindrome :: Int -> Bool
isPalindrome n = n == reverseInt n

reverseInt :: Int -> Int
reverseInt = read . reverse . show

main :: IO()
-- main = print (problem4 2) -- 9009
main = print (problem4 3) -- 906609
