import Control.Monad (replicateM)

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

table n f = [input++[f input] | input <-gen n ] 
        where gen n = replicateM n [True, False]