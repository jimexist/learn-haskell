module Tree.LLRBTree
( LLRBTree(..)
, singleton
, insert
, delete
, fromList
, toSortedList
    ) where

data LLRBTree k v =
        Empty
        | Red {leftNode::LLRBTree k v,
                pair::(k,v),
                rightNode::LLRBTree k v}
        | Black {leftNode::LLRBTree k v,
                pair::(k,v),
                rightNode::LLRBTree k v}
        deriving (Show, Eq)

singleton :: (Ord k, Eq v) => (k, v) -> LLRBTree k v
singleton p = Black Empty p Empty

insert :: (Ord k, Eq v) => (k, v) -> LLRBTree k v -> LLRBTree k v
insert ip Empty = singleton ip
insert ip (Black l op@(k,v) r) =
insert ip (Red l op@(k,v) r) =

delete :: (Ord k, Eq v) => k => LLRBTree k v - > LLRBTree k v
delete k

flipColor :: (Ord k, Eq v) => LLRBTree k v -> LLRBTree k v
flipColor (Black l p r) = Red (flipColor' l) p (flipColor' r)
flipColor (Red l p r) = Black (flipColor' l) p (flipColor' r)

flipColor' :: (Ord k, Eq v) => LLRBTree k v -> LLRBTree k v
flipColor' (Black l p r) = Red l p r
flipColor' (Red l p r) = Black l p r
flipColor' Empty = Empty
