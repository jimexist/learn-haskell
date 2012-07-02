module Tree.AVLTree
( AVLTree(..)
, singleton
, heightOf
, insert
, isBalanced
    ) where

data AVLTree k v = Empty | Branch { left :: AVLTree k v
                                  , pair :: (k, v)  
                                  , h :: Int
                                  , right :: AVLTree k v
                                  } deriving (Show, Eq)

singleton :: (k, v) -> AVLTree k v
singleton (k, v) = Branch Empty (k, v) 0 Empty

-- wrapped for Empty
heightOf :: AVLTree k v -> Int
heightOf Empty = -1
heightOf branch = h branch

insert :: (Ord k) => (k, v) -> AVLTree k v -> AVLTree k v
insert (key, val) Empty = singleton (key, val)
insert (key, val) (Branch leftNode (k, v) h rightNode) = case compare key k of
                                                    EQ -> Branch leftNode (key, val) h rightNode -- update
                                                    GT -> rotateRight . updateHeight $ Branch leftNode (k, v) h (insert (key, val) rightNode)
                                                    LT -> rotateLeft . updateHeight $ Branch (insert (key, val) rightNode) (k, v) h rightNode

rotateLeft :: AVLTree k v -> AVLTree k v
rotateLeft (Branch leftNode (k, v) h rightNode) | balanced  = Branch leftNode (k, v) h rightNode
                                                | otherwise = if isSingle
                                                                then rotateLeftSingle $ Branch leftNode (k, v) h rightNode
                                                              else rotateLeftDouble $ Branch leftNode (k, v) h rightNode
                                                where (balanced, isSingle) = ((heightOf leftNode - heightOf rightNode) < 2, heightOf (left leftNode) > heightOf (right leftNode))

rotateLeftSingle :: AVLTree k v -> AVLTree k v
rotateLeftSingle (Branch leftNode (k, v) h rightNode) = Branch (left leftNode) (pair leftNode) h (Branch Empty (k, v) (h-1) rightNode)

rotateLeftDouble :: AVLTree k v -> AVLTree k v
rotateLeftDouble (Branch leftNode (k, v) h rightNode) = rotateLeftSingle $ Branch (rotateRightSingle . right $ leftNode) (k, v) h rightNode

rotateRight :: AVLTree k v -> AVLTree k v
rotateRight (Branch leftNode (k, v) h rightNode) | balanced  = Branch leftNode (k, v) h rightNode
                                                 | otherwise = if isSingle
                                                                 then rotateRightSingle $ Branch leftNode (k, v) h rightNode
                                                               else rotateRightDouble $ Branch leftNode (k, v) h rightNode
                                                where (balanced, isSingle) = ((heightOf leftNode - heightOf rightNode) > 2, heightOf (left leftNode) < heightOf (right leftNode))

rotateRightSingle :: AVLTree k v -> AVLTree k v
rotateRightSingle (Branch leftNode (k, v) h rightNode) = Branch (Branch leftNode (k, v) (h-1) Empty) (pair rightNode) h (right rightNode)

rotateRightDouble :: AVLTree k v -> AVLTree k v
rotateRightDouble (Branch leftNode (k, v) h rightNode) = rotateRightSingle $ Branch leftNode (k, v) h (rotateLeftSingle . left $ rightNode)

updateHeight :: AVLTree k v -> AVLTree k v
updateHeight Empty = Empty
updateHeight (Branch leftNode (k, v) _ rightNode) = Branch leftNode (k, v) h rightNode where h = 1 + max (heightOf leftNode) (heightOf rightNode)

isBalanced :: AVLTree k v -> Bool
isBalanced Empty = True
isBalanced (Branch leftNode _ _ rightNode) = abs (heightOf leftNode - heightOf rightNode) <= 1 && isBalanced leftNode && isBalanced rightNode