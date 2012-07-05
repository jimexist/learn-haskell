import Test.QuickCheck
import Tree.AVLTree
import Data.List (nubBy)
import Data.Ord (comparing)

unique :: (Eq k) => [(k,v)] -> [(k,v)]
unique l = nubBy f l where f a b = (fst a == fst b)

test_singleton :: (Int, String) -> Bool
test_singleton p =
    singleton p == Branch Empty p 0 Empty

test_insert1 :: (Int, String) -> Bool
test_insert1 p =
    insert p Empty == Branch Empty p 0 Empty

test_delete1 :: (Int, String) -> (Int, String) -> Property
test_delete1 p1 p2 = fst p1 < fst p2 ==>
    delete (fst p1) (singleton p1) == Empty &&
    delete (fst p2) (fromList [p1, p2]) == Branch Empty p1 0 Empty &&
    delete (fst p1) (fromList [p2, p1]) == Branch Empty p2 0 Empty

test_delete3 :: (Int, String) -> (Int, String) -> (Int, String) -> (Int, String) -> Property
test_delete3 p1 p2 p3 p4 = fst p1 < fst p2 ==> fst p2 < fst p3 ==> fst p3 < fst p4 ==>
    delete (fst p2) (fromList [p2, p1, p3, p4]) == Branch (singleton p1) p3 1 (singleton p4) &&
    delete (fst p2) (fromList [p2, p1, p4, p3]) == Branch (singleton p1) p3 1 (singleton p4)

test_l1 :: (Int, String) -> (Int, String) -> (Int, String) -> Property
test_l1 p1 p2 p3 = fst p1 < fst p2 ==> fst p2 < fst p3 ==>
    insert p1 (insert p2 (insert p3 Empty)) == Branch (Branch Empty p1 0 Empty) p2 1 (Branch Empty p3 0 Empty)

test_l2 :: (Int, String) -> (Int, String) -> (Int, String) -> Property
test_l2 p1 p2 p3 = fst p1 > fst p2 ==> fst p1 < fst p3 ==> fst p2 < fst p3 ==>
    insert p1 (insert p2 (insert p3 Empty)) == Branch (Branch Empty p2 0 Empty) p1 1 (Branch Empty p3 0 Empty)

test_r1 :: (Int, String) -> (Int, String) -> (Int, String) -> Property
test_r1 p1 p2 p3 = fst p1 > fst p2 ==> fst p2 > fst p3 ==>
    insert p1 (insert p2 (insert p3 Empty)) == Branch (Branch Empty p3 0 Empty) p2 1 (Branch Empty p1 0 Empty)

test_r2 :: (Int, String) -> (Int, String) -> (Int, String) -> Property
test_r2 p1 p2 p3 = fst p1 < fst p2 ==> fst p1 > fst p3 ==> fst p2 > fst p3 ==>
    insert p1 (insert p2 (insert p3 Empty)) == Branch (Branch Empty p3 0 Empty) p1 1 (Branch Empty p2 0 Empty)

test_fromList :: [(Int, String)] -> Bool
test_fromList [] = True
test_fromList (p:ps) =
    insert p (fromList ps) == fromList (p:ps)

test_insert :: [(Int, String)] -> Bool
test_insert [] = True
test_insert (p:ps) =
    test_insert ps &&
    insert p (fromList ps) == fromList (p:ps)

test_delete :: [(String, String)] -> Bool
test_delete [] = True
test_delete ps = let t = fromList ps in
    foldr (delete.fst) t ps == Empty

test_count :: [(Int, String)] -> Bool
test_count p =
    count (fromList p) == length (unique p)

test_valid :: [(Int, String)] -> Bool
test_valid =
    isValid . fromList