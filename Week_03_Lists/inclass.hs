main :: IO()
main = do
    print example1
    print example2
    print example3
    print example
    print $ take 20 naturals
    print $ firstNSquares 10
    --print len
    print $ take 5 ones

example1 :: [Int]
-- example1 = [1..10] 
example1 = [x | x <- [1..10], even x]

example2 :: [(Int, Int)]
example2 = [(a, b) | a <- [1..3], b <- [10..12]] 

{-
    for a=1 to 3
        for b = 10 to 12
            (a, b)
-}

example3 :: [(Int, Int)]
example3 = [(a, b) | a <- [1..3], b <- [a+10..a+12]] 

example = head ((:) 2 [20..]) --2 * (5 + 4)

naturals = [0..]

len = length naturals

firstNSquares :: Int -> [Int]
firstNSquares n = square $ take n naturals
    where
        square [] = []
        square (x:xs) = (x*x : square(xs))

ones = 1 : ones
