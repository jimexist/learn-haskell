import Data.List (groupBy, (\\), nub, partition)
import Debug.Trace (trace)

type Node = Char
type Path = [Node]
data Edge = Edge {startNode :: Node, endNode :: Node} deriving (Show, Eq, Read)
data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq, Read)
data AdjacencyList = AdjacencyList [(Node, [Node])] deriving (Show, Eq, Read)

startWith :: Edge -> Node -> Bool
startWith (Edge n1 _) node = n1 == node

endWith :: Edge -> Node -> Bool
endWith (Edge _ n2) node = n2 == node

graphToList :: Graph -> AdjacencyList
graphToList (Graph nodes edges) =
    AdjacencyList [(node, map endNode $ filter (\edge -> edge `startWith` node) edges) | node <- nodes]

acyclicPaths :: Node -> Node -> [Edge] -> [Path]
acyclicPaths a b edges = map reverse $ filter (\path -> (head path) == b) $ acyclicPaths' edges [[a]]

acyclicPaths' :: [Edge] -> [Path] -> [Path]
acyclicPaths' edges paths = case (newPaths \\ paths) of
    [] -> paths
    otherwise -> acyclicPaths' edges $ nub (newPaths ++ paths)
    where newPaths = [(endNode edge):path | edge <- edges,
                                            path <- paths,
                                            (not $ (endNode edge) `elem` path) &&
                                            (edge `startWith` (head path))]

cycles :: Node -> [Edge] -> [Path]
cycles n edges = map reverse $ filter (\path -> head path == n) $ cycles' edges [[n]] []

cycles' :: [Edge] -> [Path] -> [Path] -> [Path]
cycles' _ [] result = result
cycles' edges paths allCycles = cycles' edges newPaths newAllCycles
    where (newCycles, newPaths) =
                partition (\path -> (head path) `elem` (tail path)) $
                    [(endNode edge):path | path <- paths, edge <- edges, edge `startWith` (head path)]
          newAllCycles = newCycles ++ allCycles


main = do
    putStrLn . show . graphToList $ Graph ['r','s','t','u','v'] [Edge 's' 'r', Edge 's' 'u', Edge 'u' 'r', Edge 'u' 's']