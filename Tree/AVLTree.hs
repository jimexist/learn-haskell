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
                                                    -- update
                                                    EQ -> Branch leftNode (key, val) h rightNode
                                                    -- insert right
                                                    GT -> let insertResult = Branch leftNode (k, v) h (insert (key, val) rightNode) in
                                                            if isBalanced insertResult then
                                                                updateHeight insertResult
                                                            else -- rotate
                                                                case compare key (fst $ pair rightNode) of
                                                                    GT -> updateHeight . rotateRightSingle $ insertResult
                                                                    LT -> updateHeight . rotateRightDouble $ insertResult
                                                    -- insert left
                                                    LT -> let insertResult = Branch (insert (key, val) leftNode) (k, v) h rightNode in
                                                            if isBalanced insertResult then
                                                                updateHeight insertResult
                                                            else -- rotate
                                                                case compare key (fst $ pair leftNode) of
                                                                    LT -> updateHeight . rotateLeftSingle $ insertResult
                                                                    GT -> updateHeight . rotateLeftDouble $ insertResult


rotateLeftSingle :: AVLTree k v -> AVLTree k v
rotateLeftSingle (Branch leftNode (k, v) h rightNode) = updateHeight $ Branch (left leftNode) (pair leftNode) h (updateHeight $ Branch Empty (k, v) (h-1) rightNode)

rotateLeftDouble :: AVLTree k v -> AVLTree k v
rotateLeftDouble (Branch leftNode (k, v) h rightNode) = rotateLeftSingle $ Branch (rotateRightSingle . right $ leftNode) (k, v) h rightNode

rotateRightSingle :: AVLTree k v -> AVLTree k v
rotateRightSingle (Branch leftNode (k, v) h rightNode) = updateHeight $ Branch (updateHeight $ Branch leftNode (k, v) (h-1) Empty) (pair rightNode) h (right rightNode)

rotateRightDouble :: AVLTree k v -> AVLTree k v
rotateRightDouble (Branch leftNode (k, v) h rightNode) = rotateRightSingle $ Branch leftNode (k, v) h (rotateLeftSingle . left $ rightNode)

updateHeight :: AVLTree k v -> AVLTree k v
updateHeight Empty = Empty
updateHeight (Branch leftNode (k, v) _ rightNode) = Branch leftNode (k, v) h rightNode where h = 1 + max (heightOf leftNode) (heightOf rightNode)

isBalanced :: AVLTree k v -> Bool
isBalanced Empty = True
isBalanced (Branch leftNode _ _ rightNode) = abs (heightOf leftNode - heightOf rightNode) <= 1 && isBalanced leftNode && isBalanced rightNode