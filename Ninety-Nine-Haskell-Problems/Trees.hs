-- trees

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Read, Eq)

data WTree a = WEmpty Int | WBranch a Int (WTree a) (WTree a) deriving (Show, Read, Eq)

flipTree Empty = Empty
flipTree (Branch e l r) = Branch e (flipTree r) (flipTree l)

symmetricTree Empty = True
symmetricTree (Branch _ l r) = l == flipTree r

construct [] 

-- graphs