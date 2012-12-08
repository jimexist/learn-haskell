
and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' a = not . and' a

nor' a = not . or' a

xor' True False = True
xor' False True = True
xor' _ _ = False

equ' a = not . xor' a

infixl 4 `or'`
infixl 6 `and'`

square a = [(x,y) | x <- a, y <- a]

power 1 a = a
power n a = let rest = foldl (\acc ele -> ele:acc) [] (power (n-1) a)
    in map (\b -> [y:x | x<-b, y<-a]) rest

table :: (Bool->Bool->Bool)->[(Bool, Bool, Bool)]
table f = map f' (square [True, False])
    where f' (a, b) = (a, b, f a b)