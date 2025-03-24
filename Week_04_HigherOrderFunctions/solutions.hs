main :: IO()
main = do
    print $ removeConsecutive [1,2,2,2,1]
    print $ removeConsecutive [1,1,1,1,1]
    print $ encode "aaaaabbba"
    print $ squareAll [1,2,3,4,5]
    print $ div3notdiv7 [1..25]
    print $ takeAllButKths [10, 3434, 45, -5] 2
    print $ takeAllButKths [10, 3434, 45, -5] 3
    print $ takeAllButKths [10, 3434, 45, -5] 1
    print $ dotProd [1,2,3] [1,1,1]
    print $ dotProd [1,0,0] [0,1,0]
    print $ split even [1,2,3,4,5,6,7]

-- 0)
removeConsecutive :: [Int] -> [Int]
removeConsecutive (x:y:xs) =
    if x == y then removeConsecutive (y:xs)
    else x:removeConsecutive (y:xs)
removeConsecutive xs = xs

-- 1)
encode :: String -> [(Char, Int)]
encode str = reverse $ helper str []
    where
        helper ""     res = res
        helper (x:xs)  [] = helper xs [(x, 1)]
        helper (x:xs) res@((sym, cnt):rs) =
            if x == sym
                then helper xs ((sym, cnt + 1):rs)
                else helper xs ((x, 1):res)

-- 2)
squareAll :: [Double] -> [Double]
-- squareAll lst = map square lst
--     where
--         square x = x * x
squareAll = map (^2)

-- 3)
div3notdiv7 :: [Int] -> [Int]
div3notdiv7 lst = filter cond lst
    where cond x = x `mod` 3 == 0 && x `mod` 7 /= 0

-- 4)
takeAllButKths :: [a] -> Int -> [(a, Int)]
takeAllButKths lst k = filter cond . zip lst $ [1..]
    where
        cond (_, idx) = idx `mod` k /= 0

-- 5)
dotProd :: [Double] -> [Double] -> Double
dotProd xs ys = foldr (+) 0 . zipWith (*) xs $ ys

-- 6)
split :: (a -> Bool) -> [a] -> ([a], [a])
split pred lst = (first, second)
    where
        first = filter pred lst
        second = filter notpred lst

        notpred x = not (pred x)

-- 7)
triags :: [Double] -> [Double] -> [Double] -> [(Double, Double, Double)]
triags xs ys zs =
    [(x, y, z) | x <- xs, y <- ys, z <- zs,
                 formTriag (x,y,z)]
    where
        formTriag (a, b, c) =
            a > 0 && b > 0 && c > 0 &&
            a < b + c && b < a + c && c < a + b 