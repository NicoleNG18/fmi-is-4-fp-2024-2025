-- 1)
applyNTimes :: (Int -> Int) -> Int -> (Int->Int)
applyNTimes f n = (foldr (.) id . map snd . map (\x -> (x, f))) [1..n]
        
-- 2)
findMinValue :: [(Int -> Int)] -> Int -> Int
findMinValue lst = \x -> (minimum . map (\f -> f x)) lst 

-- 3)
alternatingSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
alternatingSum f g n = \x -> helper 0 x
    where
        helper i prev
            | i == n    = 0
            | i `mod` 2 == 0 = f prev + helper (i+1) (f prev)
            | otherwise = g prev + helper (i+1) (g prev)

-- 4)
calcAvgGrade :: [(String, Double)] -> Double
calcAvgGrade = average . map snd
    where average lst = sum lst / fromIntegral (length lst)

-- 5)
sortGrades :: [(String, Double)] -> [(String, Double)]
sortGrades = foldr insertSorted []
    where
        insertSorted x@(name, grade) res =
            case res of
                []     -> [x]
                (r@(n, gr):rs) ->
                    if gr > grade || (gr == grade && n < name)
                        then r:insertSorted x rs
                        else x:res 

-- 6)
updatePrices :: Double -> ([Double] -> [Double])
updatePrices p = zipWith (\idx price -> price * (1 + (-1)^(idx+1) * p)) [1..]

-- 7)
calcRiemanSum :: (Double, (Double -> Double)) -> ((Double, Double) -> Double)
calcRiemanSum (eps, f) = \(a, b) -> (sum . map (*eps) . map f) [a, a + eps .. b]


main :: IO()
main = do
    print $ applyNTimes (+1) 5 5
    print $ findMinValue [(+1), (+ (-1)), (`div` 2), (*3)] 4
    print $ alternatingSum (+1) (*2) 5 2
    print $ alternatingSum (+1) (*2) 0 2
    print $ calcAvgGrade [("Pesho1", 6.0), ("Pesho2", 4.0)]
    print $ sortGrades [("Pesho1", 5.0), ("Pesho2", 6.0), ("Pesho3", 5)]
    print $ updatePrices 0.1 [1, 2, 3, 100]
    print $ calcRiemanSum (0.01, (\x -> x*x)) (0, 5)
    print $ calcRiemanSum (0.001, (\x -> x*x)) (0, 5)