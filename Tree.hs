class Tree tree where
    (==), (/=) :: tree -> tree -> Bool
    fringe, preOrder, inOrder, postOrder, levelOrder :: (Ord key) => tree -> [(key, val)]
    insert :: (Ord key) => tree -> key -> val -> Bool
    update :: (Ord key) => tree -> key -> val -> Maybe(a)
    delete :: (Ord key) => tree -> key -> Maybe val
    pop :: (Ord key) => tree -> Maybe (key, val)
    findMax, findMin :: (Ord key) => tree -> Maybe (key,val)
    findMaxkey, findMinkey :: (Ord key) => tree -> num -> Maybe [(key,val)]

data (Ord key) => BinarySearchTree bst = Nothing | Branch (BinarySearchTree bst), (key, val), (BinarySearchTree bst)

instance Tree BinarySearchTree where
    -- (==)
    Nothing == Nothing = True
    Nothing == _ = False
    _ == Nothing = False
    Branch left, pair, right == Branch l, p, r = (pair==p) && (left==l) && (right==r)

    -- fringe
    fringe Nothing = show ""
    fringe (Branch left, pair, right) = (fringe left) ++ (fringe right) || show (k, v)

    -- preOrder printing
    preOrder Nothing = show ""
    preOrder (Branch left, pair, right) = show (k, v) ++ preOrder left ++ preOrder right

    -- inOrder traverse
    inOrder Nothing = show ""
    inOrder (Branch left, pair, right) = inOrder left ++ show (k, v) ++ inOrder right

    postOrder Nothing = show ""
    postOrder (Branch left, pair, right) = postOrder left ++ postOrder right ++ show (k, v)

    insert Nothing key val = Branch Nothing, (key, val), Nothing
    insert (Branch left, pair, right) key val = case compare(pair, (key, val)) of
                                                    GT -> insert left key val
                                                    EQ -> --
                                                    LT -> insert right key val


