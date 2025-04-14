main :: IO()
main = do
    print $ treeEquals tree1 tree2
    print $ treeEquals tree1 tree1


data Tree a = Nil | Node a [Tree a]

tree1 :: Tree Int
tree1 = (Node 5 [(Node 3 [(Node 1 []),
                         (Node 2 [])]),
                (Node 4 [(Node 6 [])])
               ]
       )

tree2 :: Tree Int
tree2 = (Node 5 [(Node 3 [(Node 1 []),
                         (Node 2 [])])])

treeEquals :: Eq a => Tree a -> Tree a -> Bool
treeEquals Nil Nil = True
treeEquals Nil   _ = False
treeEquals   _ Nil = False
treeEquals tree1@(Node val1 children1) tree2@(Node val2 children2)
    | val1 /= val2                           = False
    | length children1 /= length children2   = False
    | True     = foldr (&&) True [treeEquals ch1 ch2 | (ch1, ch2) <- zip children1 children2] 
        -- children1 = [ ch1  ch2  ch3 ...  chN]
        -- children2 = [ch1' ch2' ch3' ... chN']
        -- [(ch1, ch1'), (ch2, ch2'), ... (chN, chN')]
        -- [1: True, 2: False, ... N: True] i gi agregirame s &&