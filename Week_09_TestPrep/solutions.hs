main :: IO()
main = do 
    print $ expKthTerm 100 1 -- e ^ 1 
    print $ expKthTerm 100 2 -- e ^ 2
    print $ expKthTerm 100 3 -- e ^ 3

    print $ calcCummulativeSums [1,2,0,3]
    print $ calcCummulativeSums [1,2,3,4,5]

    print $ alternatingMap [(+1), (*2)] [1,2,3,4,5]

    print $ funnyComposition [(+2),(*3),(+ (-1)),(^5)] 3
    print $ funnyComposition [(+2),(*3),(+ (-1)),(^5)] 2

    print $ consecutiveSums []
    print $ consecutiveSums [1]
    print $ consecutiveSums [1,2]
    print $ consecutiveSums [1,5,3,4,2]

    print $ (foldl (.) id [(*3), (^5)]) 3
    print $ (foldr (.) id [(*3), (^5)]) 3

-- 1)
expKthTerm :: Int -> Double -> Double
-- (sum . take k) [subiraemite]
expKthTerm k x = sum $ map fst $ take k $ iterate f (1, 0)
    where
        f (curr, idx) = (curr * x / idx', idx')
            where idx' = idx + 1    
-- (1, 0)         (f (1, 0))      (f ((f (1, 0))))
-- x^0 / 0!       1 * x / 1        x * (1 * x / 1 ) / 2

-- 3)
calcCummulativeSums :: [Int] -> [Int]
calcCummulativeSums xs = tail $ foldl f [0] xs
    where
        f result x = result ++ [last result + x]
-- [0], [0, 1], [0,1,3], [0,1,3,3], [0,1,3,3,6]

-- 4)
alternatingMap :: [a -> b] -> [a] -> [b]
alternatingMap fns xs = zipWith ($) (cycle fns) xs

-- 2)
funnyComposition :: [(Int -> Int)] -> Int -> Int
funnyComposition funcs x = (if even x then odds else evens) x
    where
        odds = foldr (.) id [f | (f, idx) <- zip funcs [1..], odd idx]
        evens = foldr (.) id [f | (f, idx) <- zip funcs [1..], even idx]


-- 28.4
consecutiveSums :: [Int] -> [Int]
consecutiveSums xs = zipWith (+) xs (tail xs)
