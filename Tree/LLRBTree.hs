module Tree.LLRBTree
( LLRBTree(..)
, singleton
, isRed
, insert
    ) where

data Color = Red | Black deriving (Show, Eq)

data LLRBTree k v = Empty | Node
                            { leftNode  :: LLRBTree k v
                            , rightNode :: LLRBTree k v
                            , pair  :: (k, v) 
                            , color :: Color
                            } deriving (Show, Eq)

singleton :: (Ord k, Eq v) => (k, v) -> LLRBTree k v
singleton p = Node Empty Empty p Black

isRed :: (Ord k, Eq v) => LLRBTree k v -> Bool
isRed (Node _ _ _ Red) = True
isRed _ = False

setColor :: (Ord k, Eq v) => Color -> LLRBTree k v -> LLRBTree k v
setColor c (Node l r p _) = Node l r p c

flipColors :: (Ord k, Eq v) => LLRBTree k v -> LLRBTree k v
flipColors n@(Node l r p c) | isBothLinks n = colorFlip (Node (colorFlip l) (colorFlip r) p c)
                            | otherwise = n
                            where   colorFlip (Node l r p Red) = Node l r p Black
                                    colorFlip (Node l r p Black) = Node l r p Red
                                    isBothLinks (Node l r _ _) = isRed l && isRed r

rotateLeft :: (Ord k, Eq v) => LLRBTree k v -> LLRBTree k v
rotateLeft n@(Node l r p c) | isRightLink n = Node (Node l (leftNode r) p Red) (rightNode r) (pair r) c
                            | otherwise = n
                            where isRightLink (Node l r _ _) = isRed l && not (isRed r)

rotateRight :: (Ord k, Eq v) => LLRBTree k v -> LLRBTree k v
rotateRight n@(Node l r p c)    | isLeftLink n = Node (leftNode l) (Node (rightNode l) r p Red) (pair l) c
                                | otherwise = n
                                where isLeftLink (Node l r _ _) = isRed r && not (isRed l)

insert :: (Ord k, Eq v) => (k, v) -> LLRBTree k v -> LLRBTree k v
insert ip = setColor Black . insertNode ip

insertNode :: (Ord k, Eq v) => (k, v) -> LLRBTree k v -> LLRBTree k v
insertNode ip Empty = singleton ip
insertNode ip node = rotateRight.rotateLeft.(do_insert ip).flipColors$node

do_insert :: (Ord k, Eq v) => (k, v) -> LLRBTree k v -> LLRBTree k v
do_insert p@(key, val) (Node l r op@(k, v) c)   | key == k  = Node l r p c
                                                | key > k   = Node l (insertNode p r) op c
                                                | key < k   = Node (insertNode p l) r op c


