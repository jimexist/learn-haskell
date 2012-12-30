import Control.Monad (liftM2)

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

-- splitWith :: (a ->Bool) ->[a] ->[[a]]
