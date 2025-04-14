import Data.Char

main :: IO()
main = do
    print $ upperLeftCorner [(-1,1),(2,2),(0,-3)]
    print $ countUppers "Hi how are you Today?"
    print $ isPerfect 6
    print $ isPerfect 123
    print $ take 2 bonus
    print $ getIndices [1,2,1,3,1,4] 1
    print $ takeChoice [(0,1),(0,1),(0,1)] [True, False, True]
    print (removeConsecutives []::[Int])
    print $ removeConsecutives [0]
    print $ removeConsecutives [0,0,0,0,0]
    print $ removeConsecutives [0,0,1,0,0]
    print $ removeConsecutives [0,0,1,1,1]
    print $ removeConsecutives [0,1,2,3,4,5]
    print $ removeConsecutives [0,0,0,0,0,1,1,1,1,5,5,5,2,2,2,5]


-- 1)
upperLeftCorner :: [(Int, Int)] -> (Int, Int)
upperLeftCorner  [] = (0,0)
upperLeftCorner pts = (minimum xs, maximum ys)
    where
        xs = map fst pts
        ys = map snd pts

-- 2)
splitBySpace :: String -> [String]
splitBySpace "" = []
splitBySpace str = word : splitBySpace (dropWhile (== ' ') rest)
    where (word, rest) = break (== ' ') str

countUppers :: String -> Int
countUppers = length . filter (\word@(c:_) -> isUpper c) . splitBySpace

-- 3)
isPerfect :: Int -> Bool
isPerfect n = n == sum [d | d <- [1..n-1], n `mod` d == 0]

bonus :: [Int]
bonus = [x | x <- [1..], isPerfect x]

-- 4)
getIndices :: Eq a => [a] -> a -> [Int]
getIndices lst x = (map fst . filter (\(_, val) -> val == x) . zip [0..]) lst

-- 5)
takeChoice :: [(a,a)] -> [Bool] -> [a]
takeChoice lst choice = [if ch then left else right | ((left, right), ch) <- zip lst choice]

-- 6)
removeConsecutives :: Eq a => [a] -> [a]
removeConsecutives (x:y:xs) = if x == y then rest else x : rest
    where rest = removeConsecutives (y:xs)
removeConsecutives lst = lst