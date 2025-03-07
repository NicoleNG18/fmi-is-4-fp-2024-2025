import Data.Char

main :: IO()
main = do
    -- print $ isWithinCircularBand (10,5) 1 100
    print $ getDigits 15
    print $ isIncreasingDigits 122345
    print $ product [1, 2, 3, 4, 5]
    print $ nthFib_iter 5
    print $ indexOf 5 [1,2,3,4,5]
    print $ reverseString "abc"
    print $ transformString "ABCD123abcd$$"

-- 1)

pi :: Double
pi = 3.14

isInsideCircle :: (Double, Double) -> Double -> Bool
isInsideCircle (x, y) r = sqrt (x**2 + y**2) < r

isOutsideCircle :: (Double, Double) -> Double -> Bool
isOutsideCircle (x, y) r = not $ isInsideCircle (x, y) r

isWithinCircularBand :: (Double, Double) -> Double -> Double -> Bool
isWithinCircularBand (x, y) r1 r2 = isOutsideCircle (x, y) r1 && isInsideCircle (x, y) r2

-- 2)
getDigits :: Int -> [Int]
getDigits n
    | n < 0                = getDigits (-n)
    | 0 <= n && n <= 9     = [n]
    | otherwise            = n `mod` 10 : getDigits (n `div` 10) -- 12345 -> [5,4,3,2,1], [1,2,3,4,5]
    -- | otherwise            = getDigits (n 'div' 10) ++ [n `mod` 10]

isInRightOrder :: [Int] -> Bool
isInRightOrder []            = True
isInRightOrder [_]           = True
isInRightOrder (x:y:xs)      = x >= y && isInRightOrder (y:xs)


isIncreasingDigits :: Int -> Bool
isIncreasingDigits n =
    isInRightOrder $ getDigits n

-- 3)
prod :: [Int] -> Int
prod []       = 1
prod (x:xs)   = x * prod xs

-- 4)
nthFib :: Int -> Int
nthFib 0     = 0
nthFib 1     = 1
nthFib n     = nthFib(n - 1) + nthFib(n - 2)

fib_helper :: Int -> Int -> Int -> Int
fib_helper   0 prev cur        = prev
fib_helper idx prev cur        = fib_helper (idx - 1) cur (prev + cur)

nthFib_iter :: Int -> Int
nthFib_iter n = fib_helper n 0 1

-- 5)
idx_helper :: Int -> Int -> [Int] -> Int
idx_helper   _ _     []     =  -1
idx_helper idx a (x:xs)     =
    if a == x then idx
    else idx_helper (idx + 1) a xs

indexOf :: Int -> [Int] -> Int
indexOf n xs = idx_helper 0 n xs

-- 6)
reverseString :: String -> String
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]

-- 7)
offset :: Int
offset = ord 'a' - ord 'A'

isLowerLetter :: Char -> Bool
isLowerLetter ch = ord 'a' <= ord ch && ord ch <= ord 'z'
isUpperLetter :: Char -> Bool
isUpperLetter ch = ord 'A' <= ord ch && ord ch <= ord 'Z'

transformSymbol :: Char -> Char
transformSymbol ch
    | isLowerLetter ch     = chr $ ord ch - offset
    | isUpperLetter ch     = chr $ ord ch + offset
    | otherwise            = ' '


transformString :: String -> String
transformString [] = []
transformString (x:xs) = transformSymbol x : transformString xs
