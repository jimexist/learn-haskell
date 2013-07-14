import Data.List
import Data.Ord
import Debug.Trace (trace)

type Node = Integer

type Path = [Node]

type Edge = (Node, Node)

-- ajacency list
data Graph = Graph [(Node, [Node])] deriving (Show, Eq, Read)

data Tree a = Leaf a | Branch {leftChild :: Tree a,
                               element :: a, 
                               rightChild :: Tree a} deriving (Show, Eq, Read)

groupByFst = groupBy (mapFst (==)) . sortBy (comparing fst)
             where mapFst f x y = f (fst x) (fst y)

toGraph :: [Edge] -> Graph
toGraph es = Graph $ map extract $ groupByFst es
             where extract es = ((fst $ head es), (map snd es))

edges :: Node -> Graph -> [Node]
edges n (Graph list) = snd . head $ dropWhile (\x -> (fst x) /= n) list

-- all paths from a to b
paths :: Node -> Node -> Graph -> [Path]
paths a b g = map reverse $ filter (\x -> b == head x) $ allNeighbors g a

neighbors :: Graph -> [Path] -> [Path]
neighbors _ [] = []
neighbors g paths = [ n:path | path <- paths, n <- edges (head path) g, n `notElem` path]

allNeighbors :: Graph -> Node -> [Path]
allNeighbors g a = foldl1 (++) $ takeWhile (not.null) $ scanl (\x f -> f x) [[a]] $ repeat (neighbors g)

-- all paths that cycle
cycle :: Node -> Graph -> [Path]
cycle a g = map (reverse . ((:) a)) $ filter connects $ allNeighbors g a
            where connects x = a `elem` (edges (head x) g)
