module Tree.BinarySearchTree
( BinarySearchTree(..)
, fringe
, preOrder
, inOrder
, postOrder
, insert
, combine
, delete
, count
, depth
, find
, findMax
, findMin
, toSortedList
, fromList
, mapTree
, filterTree
) where

import Data.Maybe (isNothing)

data BinarySearchTree k v = Empty | Branch { left :: BinarySearchTree k v
                                           , pair :: (k, v)
                                           , right :: BinarySearchTree k v
                                           } deriving (Show, Eq)

singleton :: (k, v) -> BinarySearchTree k v
singleton (k, v) = Branch Empty (k, v) Empty

-- fringe
fringe :: (Show k, Show v, Eq k, Eq v) => BinarySearchTree k v -> String
fringe Empty = ""
fringe (Branch left pair right) = if (left == Empty) && (right == Empty) then show pair else fringe left ++ fringe right

-- preOrder printing
preOrder :: (Show k, Show v) => BinarySearchTree k v -> String
preOrder Empty = ""
preOrder (Branch left (k, v) right) = show (k, v) ++ preOrder left ++ preOrder right

-- inOrder traverse
inOrder :: (Show k, Show v) => BinarySearchTree k v -> String
inOrder Empty = ""
inOrder (Branch left (k, v) right) = inOrder left ++ show (k, v) ++ inOrder right

postOrder :: (Show k, Show v) => BinarySearchTree k v -> String
postOrder Empty = ""
postOrder (Branch left (k, v) right) = postOrder left ++ postOrder right ++ show (k, v)

-- always update
insert :: (Ord k) => (k, v) -> BinarySearchTree k v -> BinarySearchTree k v
insert (k, v) Empty = Branch Empty (k, v) Empty
insert (key, val) (Branch left (k, v) right) = case compare k key of
                                                GT -> Branch (insert (key, val) left) (k, v) right
                                                EQ -> Branch left (key, val) right
                                                LT -> Branch left (k, v) (insert (key, val) right)

-- combine is asymmetrical
combine :: (Ord k) => BinarySearchTree k v -> BinarySearchTree k v -> BinarySearchTree k v
combine Empty second = second
combine first Empty = first
combine (Branch l1 (k1, v1) r1) (Branch l2 (k2, v2) r2) = case compare k1 k2 of
                                                GT -> Branch (combine l1 (Branch l2 (k2, v2) r2)) (k1, v1) r1
                                                EQ -> Branch (combine l1 l2) (k1, v1) (combine r1 r2)
                                                LT -> Branch l1 (k1, v1) (combine r1 (Branch l2 (k2, v2) r2))

delete :: (Ord k) => k -> BinarySearchTree k v -> BinarySearchTree k v
delete _ Empty = Empty
delete searchKey (Branch left (k, v) right)  = case compare k searchKey of
                                                GT -> Branch (delete searchKey left) (k, v) right
                                                EQ -> combine left right
                                                LT -> Branch left (k, v) (delete searchKey right)


count :: BinarySearchTree k v -> Int
count Empty = 0
count (Branch left _ right) = 1 + count left + count right

depth :: BinarySearchTree k v -> Int
depth Empty = 0
depth (Branch left _ right) = 1 + max (depth left) (depth right)

find :: (Ord k) => k -> BinarySearchTree k v -> Maybe v
find _ Empty = Nothing
find searchKey (Branch left (k, v) right) = case compare k searchKey of
                                                GT -> find searchKey left 
                                                EQ -> Just v
                                                LT -> find searchKey right 

findMax :: (Ord k) => BinarySearchTree k v -> Maybe (k, v)
findMax Empty = Nothing
findMax (Branch _ (k, v) right) = let bigger = findMax right in if isNothing bigger then Just (k, v) else bigger

findMin :: (Ord k) => BinarySearchTree k v -> Maybe (k, v)
findMin Empty = Nothing
findMin (Branch left (k, v) _) = let smaller = findMin left in if isNothing smaller then Just (k, v) else smaller

-- TODO asc or desc?
toSortedList :: BinarySearchTree k v -> [(k, v)]
toSortedList Empty = []
toSortedList (Branch left (k, v) right) = toSortedList left ++ [(k, v)] ++ toSortedList right

-- TODO could be more efficient?
fromList :: (Ord k) => [(k, v)] -> BinarySearchTree k v
fromList = foldr insert Empty 

mapTree :: (Ord k) => (v1 -> v2) -> BinarySearchTree k v1 -> BinarySearchTree k v2
mapTree _ Empty = Empty
mapTree fn (Branch left (k, v1) right) = Branch (mapTree fn left) (k, fn v1) (mapTree fn right)

filterTree :: (Ord k) => (v -> Bool) -> BinarySearchTree k v -> [v]
filterTree fn tree = filter fn (map snd (toSortedList tree))
