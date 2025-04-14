main :: IO()
main = do 
    -- print $ area (Circle 5.0)
    -- print $ area (Rectangle 2.0 5.0)
    -- print $ map area [Circle 5.0, Rectangle 2.0 5.0]
    -- print $ isWorkingDay Friday
    -- print $ isWorkingDay Saturday
    
    -- print $ findTotalBooks lib
    -- print $ takeAuthorsByGenre Novel lib
    
    -- print $ calcBinom (Binom 5 0)
    -- print $ calcBinom (Binom 5 1)
    -- print $ calcBinom (Binom 5 5)
    -- print $ calcBinom (Binom 5 2)
    
    -- print $ calcNat Zero
    -- print $ calcNat (Succ Zero)
    -- print $ calcNat (Succ (Succ (Succ (Succ Zero))))
    -- print $ sumNat (Zero) (Succ Zero)
    -- print $ sumNat (Succ Zero) (Succ Zero)
    -- print $ sumNat (Succ (Succ Zero)) (Succ Zero)
    -- print $ prodNat (Zero) (Succ Zero)
    -- print $ prodNat (Succ Zero) (Succ Zero)
    -- print $ prodNat (Succ (Succ (Succ Zero))) (Succ (Succ Zero))

    -- print $ depth bTree
    -- print $ depth Nil
    -- print $ depth (Node 5 Nil Nil)

    -- print $ nNodes bTree
    -- print $ nNodes Nil
    -- print $ nNodes (Node 5 Nil Nil)

    -- print $ nLeaves bTree
    -- print $ nLeaves Nil
    -- print $ nLeaves (Node 5 Nil Nil)

    -- print $ mapTree (+1) bTree
    -- print $ mapTree (*2) bTree

    -- print $ sumTree bTree
    print $ inOrderTraversal bTree
    print $ preOrderTraversal bTree
    print $ postOrderTraversal bTree
    -- print $ inOrderTraversal2 bTree

-- in class notes

data Shape = Circle Double |
             Rectangle Double Double
    deriving (Show, Eq)

-- pi :: Double
-- pi = 3.14

area :: Shape -> Double
area (Circle      r) = pi * r ^ 2
area (Rectangle a b) = a * b

-- 1)

data WeekDay = Monday | Tuesday | Wednesday |
                Thursday | Friday | Saturday | Sunday
    deriving (Eq, Show)

isWorkingDay :: WeekDay -> Bool
--isWorkingDay day = day `elem` [Monday, Tuesday, Wednesday, Thursday, Friday] 

-- isWorkingDay Saturday = False
-- isWorkingDay Sunday   = False
-- isWorkingDay _        = True

isWorkingDay day = not (day `elem` [Saturday, Sunday])

-- 2)
data Genre = Fantasy | Novel | Lyric | SciFi | Romance
    deriving (Eq, Show)
type Title = String
type Author = String
type Year = Int
type Copies = Int
data Book = Book Title Author Genre Year
type Library = [(Book, Copies)]

lib :: Library
lib = [(Book "Book1" "Author1" Novel 2000, 5),
       (Book "Book2" "Author2" SciFi 2001, 3),
       (Book "Book3" "Auhtor3" Novel 2002, 6)]

findTotalBooks :: Library -> Int
findTotalBooks = (sum . map snd)

takeAuthorsByGenre :: Genre -> Library -> [Author]
takeAuthorsByGenre gen =
    --(map fst . filter ((== gen) . snd) . map ((\ (Book title author genre year) -> (author, genre))  . fst)) lib
    (map takeAuthor . filter (\((Book _ _ g _), _) -> g == gen))
        where
            takeAuthor ((Book _ author _ _), _) = author

-- sort for home


-- 3)
data Binom = Binom Int Int

calcBinom :: Binom -> Int
calcBinom (Binom n k) = fact n `div` (fact k * fact (n-k))
    where
        fact 0 = 1
        fact n = n * fact (n - 1)

-- 4)
data Nat = Zero | Succ Nat
    deriving (Eq, Show)
-- 0 -> Zero
-- 1 -> Succ Zero
-- 2 -> Succ (Succ Zero)

-- i -> Succ (.. (Succ Zero))
calcNat :: Nat -> Int
calcNat Zero = 0
calcNat (Succ x) = 1 + calcNat x

sumNat :: Nat -> Nat -> Nat
sumNat x y = helper x valY
    where
        valY = calcNat y

        helper x 0 = x
        helper x n = helper (Succ x) (n-1)

prodNat :: Nat -> Nat -> Nat
prodNat x y = helper x valY
--prodNat x y = helper2 x valY Zero
    where
        valY = calcNat y

        -- Variant 1
        helper x 0 = Zero
        helper x y = sumNat x (helper x (y-1))
        
        -- Variant 2 - tailrec 
        helper2 _ 0 res = res
        helper2 x y res = helper2 x (y-1) (sumNat res x)

-- 5)
data BTree = Nil | Node Int BTree BTree
    deriving Show

bTree :: BTree
bTree = (Node 5 (Node 3 (Node 1 Nil Nil)
                        (Node 2 Nil Nil))
                (Node 4 (Node 8 Nil Nil)
                        Nil))

depth :: BTree -> Int
depth Nil = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

nNodes :: BTree -> Int
nNodes Nil = 0
nNodes (Node _ left right) = 1 + (nNodes left) + (nNodes right)

nLeaves :: BTree -> Int
nLeaves Nil = 0
nLeaves (Node _ Nil Nil) = 1
nLeaves (Node _ left right) = (nLeaves left) + (nLeaves right)

mapTree :: (Int -> Int) -> BTree -> BTree
mapTree _                   Nil = Nil
mapTree f (Node val left right) = (Node (f val) leftt rightt)
    where
        leftt  = mapTree f  left
        rightt = mapTree f right

foldTree :: (Int -> a -> a -> a) -> a -> BTree -> a
foldTree _ defVal                   Nil = defVal
foldTree f defVal (Node val left right) = f val leftVal rightVal
    where
        leftVal  = foldTree f defVal  left
        rightVal = foldTree f defVal right

sumTree :: BTree -> Int
sumTree = foldTree (\ val left right -> val + left + right) 0

inOrderTraversal :: BTree -> [Int]  -- left-root-right
inOrderTraversal Nil = []
inOrderTraversal (Node val left right) = leftt ++ [val] ++ rightt
    where
        leftt  = inOrderTraversal  left
        rightt = inOrderTraversal right

preOrderTraversal :: BTree -> [Int]  -- root-left-right
preOrderTraversal Nil = []
preOrderTraversal (Node val left right) = [val] ++ leftt ++ rightt
    where
        leftt  = preOrderTraversal  left
        rightt = preOrderTraversal right


postOrderTraversal :: BTree -> [Int]  -- left right root
postOrderTraversal Nil = []
postOrderTraversal (Node val left right) = leftt ++ rightt ++ [val]
    where
        leftt  = postOrderTraversal  left
        rightt = postOrderTraversal right

inOrderTraversal2 :: BTree -> [Int]
inOrderTraversal2 = foldTree (\val left right -> left ++ [val] ++ right) []
