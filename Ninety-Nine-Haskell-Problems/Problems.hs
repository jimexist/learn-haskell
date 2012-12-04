import Test.QuickCheck
import Data.Char

myLast (l:[]) = l
myLast (_:xs) = myLast xs

myButLast (a:_:[]) = a
myButLast (_:xs) = myButLast xs

myElementAt (x:_) 1 = x
myElementAt (_:xs) n | n > 0 = myElementAt xs (n-1)

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myListEq [] [] = True
myListEq [] _ = False
myListEq _ [] = False
myListEq (x:xs) (y:ys) = x == y && myListEq xs ys

myIsPalindrome s = myListEq s (myReverse s)

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a] 
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = 
    myFlatten x ++ myFlatten (List xs)

myCompress [] = []
myCompress (x:y:xs) | x == y = myCompress (x:xs)
myCompress (x:xs) = x:(myCompress xs)

myPack' :: Eq a => [[a]] -> [a] -> ([[a]], [a])
myPack' acc [] = (acc, [])
myPack' [] (e:es) = myPack' [[e]] es
myPack' (lst:rst) (e:es) =
    if (head lst == e)
    then myPack' ((e:lst):rst) es
    else myPack' ([e]:lst:rst) es

myPack :: Eq a => [a] -> [[a]]
myPack = reverse . fst . myPack' []

myEncode s = map (\e -> (length e, head e)) $ myPack s

data Encoded a = Single a | Multiple Int a deriving (Show)

myEncodeModified s = 
    map lambda $ myPack s
    where lambda = \e -> if (length e == 1) then Single (head e) else Multiple (length e) (head e)

myDecode [] = []
myDecode ((Single a):xs) = a:(myDecode xs)
myDecode ((Multiple n a):xs) = (multiple n a)++(myDecode xs)

multiple 0 _ = []
multiple n a = a:(multiple (n-1) a)

myDupli [] = []
myDupli (x:xs) = x:x:(myDupli xs)

myRepli [] _ = []
myRepli _ 0 = []
myRepli (x:xs) n = (multiple n x) ++ (myRepli xs n)

myDrop' _ [] _ = []
myDrop' acc (x:xs) counter =
    if (acc `mod` counter == 0)
    then rest
    else x:rest
    where rest = myDrop' (acc+1) xs counter

myDrop s counter = myDrop' 1 s counter

mySplitAt' lhs rhs 0 = (reverse lhs, rhs)
mySplitAt' lhs (x:xs) n = mySplitAt' (x:lhs) xs (n-1)

mySplitAt s n = mySplitAt' [] s n

mySlice s begin end = 
    snd $ mySplitAt (fst $ mySplitAt s end) (begin-1)

myRotate s 0 = s
myRotate s n | n > 0 = flipJoin $ mySplitAt s n
    where flipJoin (a, b) = b ++ a
myRotate s n = myRotate s (n + length s)

myRemoveAt s n | n > length s || n <=0 = s
myRemoveAt (_:xs) 1 = xs
myRemoveAt (x:xs) n = x:rest
    where rest = myRemoveAt xs (n-1)

-- tests

testLast s =
    length s > 0 ==>
    myLast s == (head $ reverse s)
    where types = s::[Char]

testButLast s =
    length s > 1 ==>
    myButLast s == (head $ tail $ reverse s)
    where types = s::[Char]

testElementAt s n =
    n > 0 && length s > n ==>
    myElementAt s n == s !! (n-1)
    where types = s::[Char]

testMyLength s =
    myLength s == length s 
    where types = s::[Char]

testMyReverse s = 
    myReverse s == reverse s 
    where types = s::[Char]

main = do
    quickCheck testLast
    quickCheck testButLast
    quickCheck testElementAt
    quickCheck testMyLength
    quickCheck testMyReverse