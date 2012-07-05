module Tree.AVLTree
( AVLTree(..)
, singleton
, heightOf
, count
, insert
, delete
, findMax
, findMin
, isValid
, fromList
    ) where

import Data.Maybe (fromJust)

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

insert :: (Ord k, Eq v, Show k, Show v) => (k, v) -> AVLTree k v -> AVLTree k v
insert (key, val) Empty = singleton (key, val)
insert (key, val) (Branch leftNode (k, v) h rightNode) = case compare key k of
    -- update
    EQ -> Branch leftNode (key, val) h rightNode
    -- insert right
    GT -> let insertResult = Branch leftNode (k, v) h (insert (key, val) rightNode) in
            if isBalanced insertResult then
                updateHeight insertResult
            else -- rotate
                case compare key (fst.pair.right$insertResult) of
                    GT -> rotateRightSingle insertResult
                    LT -> rotateRightDouble insertResult
                    EQ -> error "GT"
    -- insert left
    LT -> let insertResult = Branch (insert (key, val) leftNode) (k, v) h rightNode in
            if isBalanced insertResult then
                updateHeight insertResult
            else -- rotate
                case compare key (fst.pair.left$insertResult) of
                    LT -> rotateLeftSingle insertResult
                    GT -> rotateLeftDouble insertResult
                    EQ -> error "LT"

delete :: (Ord k, Eq v) => k -> AVLTree k v -> AVLTree k v
delete _ Empty = Empty
delete key (Branch leftNode (k, v) h rightNode) = case compare key k of
    GT ->   updateHeight $ Branch leftNode (k, v) h (delete key rightNode)
    LT ->   updateHeight $ Branch (delete key leftNode) (k, v) h rightNode
    EQ ->   if leftNode == Empty
                then rightNode -- both Empty then rightNode yields Empty
            else if rightNode == Empty 
                then leftNode
            else
                let deleteResult = updateHeight $ Branch (delete (fst prev) leftNode) prev h rightNode where prev=fromJust.findMax$leftNode in -- find the previous one
                if isBalanced deleteResult then
                    deleteResult
                else case compareHeights . right $ deleteResult of
                    GT -> rotateRightDouble deleteResult
                    LT -> rotateRightSingle deleteResult
                    EQ -> rotateRightSingle deleteResult -- either single or double works

count :: AVLTree k v -> Int
count Empty = 0
count (Branch leftNode _ _ rightNode) = count leftNode + count rightNode + 1
            

findMax :: (Ord k, Eq v) => AVLTree k v -> Maybe (k, v)
findMax Empty = Nothing
findMax (Branch _ (k, v) _ rightNode) | rightNode == Empty = Just (k, v)
                                      | otherwise = findMax rightNode

findMin :: (Ord k, Eq v) => AVLTree k v -> Maybe (k, v)
findMin Empty = Nothing
findMin (Branch leftNode (k, v) _ _) | leftNode == Empty = Just (k, v)
                                     | otherwise = findMin leftNode

compareHeights :: AVLTree k v -> Ordering
compareHeights Empty = EQ
compareHeights (Branch leftNode _ _ rightNode) = compare (heightOf leftNode) (heightOf rightNode)

rotateLeftSingle :: (Ord k, Eq v) => AVLTree k v -> AVLTree k v
rotateLeftSingle (Branch leftNode p _ rightNode) = 
    updateHeight$Branch 
        (left leftNode) 
        (pair leftNode) 
        0
        (updateHeight$Branch (right leftNode) p 0 rightNode)

rotateLeftDouble :: (Ord k, Eq v) => AVLTree k v -> AVLTree k v
rotateLeftDouble (Branch leftNode p _ rightNode) = 
    updateHeight$Branch
        (updateHeight$Branch (left leftNode) (pair leftNode) 0 (left.right$leftNode))
        (pair.right$leftNode)
        0
        (updateHeight$Branch (right.right$leftNode) p 0 rightNode)

rotateRightSingle :: (Ord k, Eq v) => AVLTree k v -> AVLTree k v
rotateRightSingle (Branch leftNode p _ rightNode) =
    updateHeight$Branch 
        (updateHeight$Branch leftNode p 0 (left rightNode)) 
        (pair rightNode) 
        0 
        (right rightNode)

rotateRightDouble :: (Ord k, Eq v) => AVLTree k v -> AVLTree k v
rotateRightDouble (Branch leftNode p _ rightNode) = 
    updateHeight$Branch
        (updateHeight$Branch leftNode p 0 (left.left$rightNode))
        (pair.left$rightNode)
        0
        (updateHeight$Branch (right.left$rightNode) (pair rightNode) 0 (right rightNode))

updateHeight :: AVLTree k v -> AVLTree k v
updateHeight Empty = Empty
updateHeight (Branch leftNode (k, v) _ rightNode) = Branch leftNode (k, v) h rightNode
    where h = 1 + max (heightOf leftNode) (heightOf rightNode)

isBalanced :: AVLTree k v -> Bool
isBalanced Empty = True
isBalanced (Branch leftNode _ _ rightNode) = abs (heightOf leftNode - heightOf rightNode) <= 1 -- non-recursive

isValid :: AVLTree k v -> Bool
isValid Empty = True
isValid (Branch leftNode p h rightNode) = 
    isBalanced (Branch leftNode p h rightNode) &&
    h == 1 + max (heightOf leftNode) (heightOf rightNode) &&
    isValid leftNode && 
    isValid rightNode

fromList :: (Ord k, Eq v, Show k, Show v) => [(k, v)] -> AVLTree k v
fromList = foldr insert Empty 