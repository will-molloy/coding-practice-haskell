-- https://projecteuler.net/problem=9

problem9 :: Int -> Int
problem9 n = product (pythagoreanTripletThatSumsTo n)

pythagoreanTripletThatSumsTo :: Int -> [Int]
pythagoreanTripletThatSumsTo n = head [[a,b,c] | a <- [1..(n-2)], 
                                                 b <- [(a+1)..(n-a-1)], 
                                                 let c = n - a - b, 
                                                 a^2 + b^2 == c^2]

main :: IO()
-- main = print (problem9 12) -- 60
main = print (problem9 1000) -- 31875000
