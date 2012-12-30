import Control.Monad (liftM2)
import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:y) = Just y

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (y:[]) = Just y
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just [x]
safeInit (x:xs) = liftM2 (:) (Just x) (safeInit xs)

splitWith :: (a ->Bool) ->[a] ->[[a]]
splitWith f s = case dropWhile (not . f) s of
                        []  -> []
                        s'  -> w : splitWith f s''
                               where (w, s'') = break (not . f) s'

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x:[h|(h:_) <-xss]) : transpose(xs:[t|(_:t)<-xss])

transposeFile :: [Char] ->[Char]
transposeFile = unlines . transpose . lines

asInt_fold :: String ->Int
asInt_fold ('-':cs) = (- (asInt_fold' cs))
asInt_fold cs = asInt_fold' cs

asInt_fold' = foldl (\acc ele ->10*acc + (digitToInt ele)) 0

