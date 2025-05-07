main :: IO()
main = do
    print "hello"

    print $ condV [(1, 2, 2), (1, 2, 5)]
    print $ condG [(1,1,1), (2,1,2)]
    print $ condG [(2,2,2), (2,2,2)]
    print $ condG [(1,2,3), (2,1,2)]

    print $ cummulativeSums [1,2,3]

    print $ ((createfn [(1,1), (2,2), (3,3)]) 2)
    print $ (caseof [(\x -> x `mod` 3 == 0), (\x -> x `mod` 3 == 1)] [(\x -> x + 1), (\x -> x - 1)]) 5
    print $ (caseof [even, odd] [(\x -> x + 1), (\x -> x - 1)]) 5

-- 28.2
    -- a
sums :: [(Int, Int, Int)] -> [Int]
sums = map (\(x, y, z) -> x + y + z)
sums2 :: [(Int, Int, Int)] -> [Int]
sums2 lst = [ x+y+z |(x, y, z) <- lst]

--     -- b
sumByComponents :: [(Int, Int, Int)] -> (Int, Int, Int)
sumByComponents = foldr (\(x,y,z) (res_x, res_y, res_z) -> (res_x+x, res_y+y, res_z+z)) (0,0,0)

sumByComponents2 lst = (sum xs, sum ys, sum zs)
    where
        xs = map (\(x,_,_) -> x) lst
        ys = map (\(_,y,_) -> y) lst
        zs = map (\(_,_,z) -> z) lst


condV :: [(Int, Int, Int)] -> Int
condV = length . filter (\(a,b,c) -> a+b>c)

condG :: [(Int, Int, Int)] -> Bool
condG = any (\(a,b,c) -> a == b && b == c) 


-- 28.5
cummulativeSums :: [Int] -> [Int]
cummulativeSums [] = []
cummulativeSums (x:xs) = x : zipWith (+) (cummulativeSums (x:xs)) xs
-- 28.4
-- addNeighbours (x:xs) = zipWith (+) (x:xs) xs


-- 29.2
createfn :: Eq a => [(a, b)] -> a -> b
createfn lst = (\a -> (snd . head .filter (\(x,_) -> x == a)) lst)

-- 29.1
caseof :: [a -> Bool] -> [a -> b] -> a -> b
caseof preds funcs = \x ->
    let results = map (\(p, f) -> (p x, f x)) (zip preds funcs)
        matches = filter fst results
    in if length matches > 0
          then (snd . head) matches
          else (snd . last) results

