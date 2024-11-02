import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]

buildAdjList :: RoadMap -> AdjList
buildAdjList roadmap = foldl add2ToAdjList [] roadmap

add2ToAdjList :: AdjList -> (City,City,Distance) -> AdjList
add2ToAdjList adjList (city1,city2,dist) = add1ToAdjList (add1ToAdjList adjList (city1,city2,dist)) (city2, city1, dist)

add1ToAdjList :: AdjList -> (City,City,Distance) -> AdjList
add1ToAdjList [] (city1,city2,distance) = [(city1,[(city2,distance)])]
add1ToAdjList ((c1,c_adj):xs) (city1,city2,distance) = if c1 == city1 then (c1, c_adj ++ [(city2,distance)]):xs else (c1,c_adj) : add1ToAdjList xs (city1, city2, distance)

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(n1, n2, _) -> (n1 == city1 && n2 == city2) || (n1 == city2 && n2 == city1)) roadMap

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, dist):xs) city1 city2 = if ((c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) then Just dist else distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap origin = [(city, dist) | (city1,city2,dist) <- roadMap, city <- if origin == city1  then [city2] else if origin == city2 then [city1] else [] ]

adjacentAdjList :: AdjList -> City -> [(City,Distance)]
adjacentAdjList adjList city = case lookup city adjList of
                                    Just neighbors -> neighbors
                                    Nothing -> []

getNeighbors :: AdjList -> City -> [City]
getNeighbors adjList city = case lookup city adjList of
                                Just neighbors -> map fst neighbors
                                Nothing -> []

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = length (dfs adjList origin []) == length (cities roadmap)
    where
        adjList = buildAdjList roadmap
        origin = case roadmap of
                    ((firstCity,_,_) : _) -> firstCity
                    [] -> error "There are no edges"


dfs :: AdjList -> City -> [City] -> [City]
dfs adjList newCity visited
    | elem newCity visited = visited
    | otherwise = foldl (\visitedList neighbor -> dfs adjList neighbor visitedList) (newCity : visited) neighbors
    where
        neighbors = getNeighbors adjList newCity


bfs :: City -> City -> AdjList -> [Path]
bfs c1 c2 adjList = bfsAux [(0,[c1])] [] maxInt
    where
        maxInt = maxBound :: Int
        bfsAux :: [(Distance, Path)] -> [Path] -> Int -> [Path]
        bfsAux [] shortestPaths _ = shortestPaths
        bfsAux ((dist, path@(current:_)):queue) shortestPaths minDistance
            | current == c2 && ( null shortestPaths || dist < minDistance) = bfsAux queue [path] dist
            | current == c2 && dist == minDistance = bfsAux queue (path : shortestPaths) minDistance
            | current == c2 = bfsAux queue shortestPaths minDistance
            | otherwise =
                let neighbors = adjacentAdjList adjList current
                    newPaths  = [(dist + d, nextCity : path) | (nextCity, d) <- neighbors, not (elem nextCity path)]
                in bfsAux (queue ++ newPaths) shortestPaths minDistance


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap c1 c2
    | c1 == c2 = [[c1]]
    | otherwise = bfs c1 c2 (buildAdjList roadMap)


travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- unconnected graph
gTest4 = [("0","1",1),("0","2",2),("1","3",2),("2","3",1)]