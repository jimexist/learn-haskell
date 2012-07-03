import Test.QuickCheck
import Tree.BinarySearchTree
import Data.List (sort, nub, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

prop_singleton :: (String, String) -> Bool
prop_singleton (k, v) = 
    delete k (singleton (k, v)) == Empty

prop_inputOrderNonrelevant :: [(String, String)] -> Bool
prop_inputOrderNonrelevant ps =
    fromList ps == fromList (sortBy (comparing fst) ps)

prop_listInvariant :: [(String, String)] -> Bool
prop_listInvariant ks = 
    let keysOf = nub . map fst in keysOf (toSortedList.fromList$ks) == sort (keysOf ks)

prop_singleInsert :: (String, String) -> Bool
prop_singleInsert (k, v) =
    insert (k, v) Empty == singleton (k, v)

prop_skewInsert :: [(String, String)] -> Bool
prop_skewInsert ps = 
    (depth.fromList$sortBy (comparing fst) ps) ==
    (depth.fromList.reverse$ sortBy (comparing fst) ps)

prop_listSorted :: [(String, String)] -> Bool
prop_listSorted ps =
    map fst (toSortedList (fromList ps)) == sort (nub (map fst ps))

prop_findMax :: [(String, String)] -> Property
prop_findMax ps = not (null ps) ==>
    (fromJust.findMax.fromList $ ps) == last (sortBy (comparing fst) ps)

prop_findMin :: [(String, String)] -> Property
prop_findMin ps = not (null ps) ==>
    (fromJust.findMin.fromList $ ps) == head (sortBy (comparing fst) ps)

prop_insertDelete :: (String, String) -> [(String, String)] -> Bool
prop_insertDelete p ps =
    delete (fst p) (insert p (fromList ps)) == fromList ps