import Data.List (groupBy, (\\), union, partition, delete)
import Debug.Trace (trace)

type Node = Char
type Path = [Node]
data Edge = Edge {startNode :: Node, endNode :: Node} deriving (Show, Eq, Read)
data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq, Read)
data AdjacencyList = AdjacencyList [(Node, [Node])] deriving (Show, Eq, Read)
data Tree a = Leaf a | Branch {leftChild :: Tree a, element :: a, rightChild :: Tree a} deriving (Show, Eq, Read)

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
    otherwise -> acyclicPaths' edges (paths `union` newPaths)
    where newPaths = [(endNode edge):path | edge <- edges,
                                            path <- paths,
                                            ((endNode edge) `notElem` path) &&
                                            (edge `startWith` (head path))]

cycles :: Node -> [Edge] -> [Path]
cycles n edges = map reverse $ filter (\path -> head path == n) $ cycles' edges [[n]] []

cycles' :: [Edge] -> [Path] -> [Path] -> [Path]
cycles' _ [] result = result
cycles' edges paths allCycles = cycles' edges newPaths (allCycles ++ newCycles)
    where (newCycles, newPaths) = partition (\path -> (head path) `elem` (tail path))
            [(endNode edge):path | path <- paths, edge <- edges, edge `startWith` (head path)]

isConnected :: Graph -> Bool
isConnected (Graph nodes edges) = all isConnected' nodes
    where isConnected' node = node `elem` edgeNodes
          edgeNodes = (map startNode edges) `union` (map endNode edges)

-- directed and undirected graphs have different defitions of cycle
hasCycle :: Graph -> Bool
hasCycle (Graph nodes edges) = any isConnected [Graph nodes (delete edge edges) | edge <- edges]

isTree :: Graph -> Bool
isTree graph = (isConnected graph) && (not $ hasCycle graph)

spanningTrees :: Graph -> [Graph]
spanningTrees (Graph nodes edges) = undefined

spanningTrees' :: Graph -> [Graph]
spanningTrees' (Graph nodes edges) = undefined

depthFirstSearch :: Graph -> Node -> (Node -> Bool) -> Maybe Node
depthFirstSearch graph root predicate = undefined

breadthFirstSearch :: Graph -> Node -> (Node -> Bool) -> Maybe Node
breadthFirstSearch graph root predicate = bfs (edges graph) [root] predicate

bfs :: [Edge] -> [Node] -> (Node -> Bool) -> Maybe Node
bfs _ [] _ = Nothing
bfs edges (n:ns) predicate = if (predicate n) then Just n else bfs edges (ns `union` newNodes) predicate
    where newNodes = [endNode edge | edge <- edges, edge `startWith` n]

main = do
    putStrLn . show . graphToList $ Graph ['r','s','t','u','v'] [Edge 's' 'r', Edge 's' 'u', Edge 'u' 'r', Edge 'u' 's']