main :: IO()
main = do
    print $ remove_duplicates [1,1,1,1,5,2,1, 2,1] -- [1,5,2]
    print $ takeFirstN [1,2,3,4,5] 3 -- ([1,2,3],[4,5])
    print $ simpleDecode [('a', 2), ('b', 3), ('#', 5)] -- "aabbb#####"
    print $ merge [-1,2,3] [1,2,2,2,3,4] -- [1,1,2,2,2,2,3,3,4]
    print $ quickSort [1,2,3,1,2,4,-5,0] -- [-5,0,1,1,2,2,3,4]
    print $ split [1,2,3,4,5,6,7] 2 -- [[1,2],[3,4],[5,6]]
    print $ trimSpaces "   1 2 3 abcd a " -- "1 2 3 abcd a"
    print $ firstNPrimes1 10 -- [2,3,5,7,11,13,17,19,23,29]
    print $ firstNPrimes2 10 -- [2,3,5,7,11,13,17,19,23,29]

-- 0)

remove_duplicates :: [Int] -> [Int]
remove_duplicates [] = []
remove_duplicates (x:xs) = x: remove_duplicates (remove x xs)
    where 
        remove _     [] = []
        {-
            грешката, която излезе е, че на следващия ред вместо да закача у към резултата,
            аз записвах x и не го видях навреме.

            отново се извинявам за бъгването :)
        -}    
        remove x (y:ys) = if x == y then removed else (y : removed)
            where
                removed = remove x ys

-- 1)
takeFirstN :: [a] -> Int -> ([a], [a])
takeFirstN []     _ = ([],[])
takeFirstN xs     0 = ([], xs)
takeFirstN (x:xs) n = (x:heads, tail)
    where
        (heads, tail) = takeFirstN xs (n-1)

-- 2)
simpleDecode :: [(Char, Int)] -> String
simpleDecode []     = ""
simpleDecode (x:xs) = helper x ++ simpleDecode xs
    where
        helper (  _, 0) = ""
        helper (sym, n) = sym : helper (sym, n-1)
        
-- 3)
merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge xxs@(x:xs) yys@(y:ys)
    | x <= y    = x : merge  xs yys
    | otherwise = y : merge xxs  ys

-- 4)
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = lessOrEq ++ [x] ++ bigger
    where
        lessOrEq = quickSort [y | y <- xs, y <= x]
        bigger   = quickSort [y | y <- xs, y  > x]

-- 5)
split :: [a] -> Int -> [[a]]
split [] _ = []
split xs n 
    | length heads < n = []
    | otherwise        = heads : split tail n
    where
        (heads, tail) = takeFirstN xs n

-- 6)
trimSpaces :: String -> String
trimSpaces [] = []
trimSpaces (' ':str) = trimSpaces str
trimSpaces (s:' ':' ':str) = trimSpaces (s:' ':str)
trimSpaces (s:' ':s':str) = s:' ':trimSpaces (s':str)
trimSpaces (s:str) = s: trimSpaces str


-- 7)
naturals = [2..]

    -- вариант 1: Минаваме последователно през безкрайния списък и вземаме само числата, за които е изпълнено isPrime
firstNPrimes1 n = take n [x | x <- naturals, isPrime x]
    where
        isPrime x = [] == [div | div <- divisors, x `mod` div == 0]
            where divisors = [2..floor(sqrt(fromIntegral(x)))]

    -- вариант 2: Чрез решето на Ератостен, което генерираме чрез безкраен списък
firstNPrimes2 :: Int -> [Int]
firstNPrimes2 n = take n (sieve naturals)
    where sieve (x:xs) = x: sieve [m | m <- sieve xs, m `mod` x /= 0]

 