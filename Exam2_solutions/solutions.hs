import Data.List(nub)

main :: IO()
main = do
    print "Problem 1:"
    print $ countCommonElements [1,2,3] [1,2,3]
    print $ countCommonElementsWithRepeating [1,2,3] [1,2,3,2,2]
    print $ countCommonElementsWithRepeating [1,2,3,3,3,3] [1,2,3,2,2]
    print $ countCommonElementsWithRepeating [1,2,3,1,1] [1,2,3]
    print $ countCommonElementsWithRepeating [1,2,3] [1,2,3]
    print $ countCommonElementsWithRepeating2 [1,2,3] [1,2,3,2,2]
    print $ countCommonElementsWithRepeating2 [1,2,3,3,3,3] [1,2,3,2,2]
    print $ countCommonElementsWithRepeating2 [1,2,3,1,1] [1,2,3]
    print $ countCommonElementsWithRepeating2 [1,2,3] [1,2,3]
    print "Problem 2:"
    print $ findAvgHeight people
    print $ getTallest people
    print $ getTallest []
    print $ hasMultipleBirthyears people
    print $ hasMultipleBirthyears []
    print "Problem 3:"
    print $ hasFixedPoint [(\x -> x+1), (\x -> x-1)] 5
    print $ hasFixedPoint [(\x -> x+1), (\x -> x-1), id] 5
    print $ hasFixedPoint [(\x -> x+1), (\x -> x-1), (\x -> (x + 5) `div` 2)] 5
    print "Problem 4:"
    print $ isSquareMatrix []
    print $ isSquareMatrix [[1]]
    print $ isSquareMatrix [[1,2], [3,4]]
    print $ isSquareMatrix [[1,2], [1]]
    print $ isSquareMatrix [[1],[2],[3]]
    print $ isSquareMatrix [[1,2,3],[4,5,6],[7,8,9]]
   
-- 1.1
countCommonElements :: Eq a => [a] -> [a] -> Int
countCommonElements xs = length . filter (`elem` xs)

-- 1.2
countCommonElementsWithRepeating :: Eq a => [a] -> [a] -> Int
countCommonElementsWithRepeating xs ys = countCommonElements xs (nub ys)
    -- nub is a built in function that removes duplicates from a list

countCommonElementsWithRepeating2 :: Eq a => [a] -> [a] -> Int
countCommonElementsWithRepeating2 xs ys = countCommonElements xs (removeDuplicates ys)
    where
        removeDuplicates [] = []
        removeDuplicates (x:xs) = if x `elem` xs then xss else x:xss
            where xss = removeDuplicates xs


-- 2.1
data Person = Person {name :: String, birthyear :: Int, height :: Int}

people = [Person {name = "Ivan",  birthyear = 1999, height = 185},
          Person {name = "Petar", birthyear = 2002, height = 179},
          Person {name = "Maria", birthyear = 2003, height = 182},
          Person {name = "Noob", birthyear = 2003, height = 120}]

-- 2.2
findAvgHeight :: [Person] -> Int
findAvgHeight [] = 0
findAvgHeight ps = (sum . map height) ps `div` length ps

-- 2.3
getTallest :: [Person] -> [String]
getTallest ps = (map name . filter (\(Person _ _ height) -> height > avgHeight)) ps
    where avgHeight = findAvgHeight ps

-- 2.4
hasMultipleBirthyears :: [Person] -> Bool
hasMultipleBirthyears ps = length ps > uniqueBirthyears
    where uniqueBirthyears = (length . nub . map birthyear) ps

-- 3
hasFixedPoint :: Eq a => [a -> a] -> a -> Bool
hasFixedPoint fs x = any (\f -> f x == x) fs

-- 4
isSquareMatrix :: [[a]] -> Bool
isSquareMatrix xss = rows >= 2 && all (\xs -> length xs == rows) xss
    where rows = length xss