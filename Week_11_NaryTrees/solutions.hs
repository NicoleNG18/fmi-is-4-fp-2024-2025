main :: IO()
main = do
    print "hello"
    print $ findDepth empty
    print $ findDepth root
    print $ findDepth tree

    print $ foldTree (\val lst -> val + sum lst) 0 empty
    print $ foldTree (\val lst -> val + sum lst) 0 root
    print $ foldTree (\val lst -> val + sum lst) 0 tree

    print $ foldTree (\val lst -> val : concat lst) ([]::[Int]) empty
    print $ foldTree (\val lst -> val : concat lst) [] root
    print $ foldTree (\val lst -> val : concat lst) [] tree

    print $ getKthLevel 1 tree
    print $ getKthLevel 2 tree
    print $ getKthLevel 3 tree
    print $ getKthLevel 4 tree
    print $ getKthLevel 5 tree
    print $ getKthLevel 6 tree
    print $ getKthLevel 10 tree

    print $ containsWord "abc" charTree

charTree = Node 'a' [Node 'b' [Node 'c' [], Node 'd' []], Node 'c' []]

data Tree a = Nil | Node a [Tree a]
    deriving (Show)

empty = Nil
root = Node 1 []
tree = Node 1 [Node 2 [Node 5 [Node  9 [Node 14 [],
                                        Node 15 []],
                               Node 10 [Node 16 []],
                               Node 11 [],
                               Node 12 []],
                       Node 6 []],
               Node 3 [Node 7 [],
                       Node 8 [Node 13 []]],
               Node 4 []]

findDepth :: Tree a -> Int
findDepth Nil = 0 -- taka sme se razbrali
findDepth (Node _ []) = 1
findDepth (Node _ children) = 1 + (maximum $ map findDepth children)

foldTree :: (a -> [b] -> b) -> b -> Tree a -> b
foldTree _ null Nil = null
foldTree f null (Node value children) =
    f value (map (foldTree f null) children)


getKthLevel :: Int -> Tree a -> [a]
getKthLevel _ Nil = []
getKthLevel 1 (Node val _) = [val]
getKthLevel k (Node _ children) = concat $ map (getKthLevel (k-1)) children


containsWord :: [Char] -> Tree Char -> Bool
containsWord  ""   _ = True
containsWord   _ Nil = False
containsWord (s:ss) (Node letter children)
    | letter /= s    = False
    | True = foldr (||) (map (containsWord ss) children)