module Tree.BinarySearchTree
( fringe
, preOrder
, inOrder
, postOrder
, insert
) where

data BinarySearchTree k v = Empty |  Branch {left :: BinarySearchTree k v, pair :: (k, v), right :: BinarySearchTree k v} deriving (Show, Eq)

-- fringe
fringe :: (Show k, Show v) => BinarySearchTree k v -> String
fringe Empty = ""
fringe (Branch left pair right) = (fringe left) ++ (fringe right) ++ show pair

-- preOrder printing
preOrder Empty = show ""
preOrder (Branch left (k, v) right) = show (k, v) ++ preOrder left ++ preOrder right

-- inOrder traverse
inOrder Empty = show ""
inOrder (Branch left (k, v) right) = inOrder left ++ show (k, v) ++ inOrder right

postOrder Empty = show ""
postOrder (Branch left (k, v) right) = postOrder left ++ postOrder right ++ show (k, v)

insert :: (Ord k) => BinarySearchTree k v -> (k, v) -> BinarySearchTree k v
insert Empty (k, v) = Branch Empty (k, v) Empty
insert (Branch left (k, v) right) (key, val) = case compare k key of
                                                GT -> Branch (insert left (key, val)) (k, v) right
                                                EQ -> if key /= k then Branch left (key, val) right else Branch left (k, v) right
                                                LT -> Branch left (k, v) (insert right (key, val))