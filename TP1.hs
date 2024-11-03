import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--Graph representation through an Adjacency list
type AdjList = [(City,[(City,Distance)])]

--Build an Adjacency List from a Roadmap
buildAdjList :: RoadMap -> AdjList
buildAdjList roadmap = foldl add2ToAdjList [] roadmap   --

--Add 2 directed Edge to an Adjacency list in order to make 1 undirected Edge  
add2ToAdjList :: AdjList -> (City,City,Distance) -> AdjList
add2ToAdjList adjList (city1,city2,dist) = add1ToAdjList (add1ToAdjList adjList (city1,city2,dist)) (city2, city1, dist)

--Add an directed Edge to an Adjacency list, add Vertex if not yet represented 
add1ToAdjList :: AdjList -> (City,City,Distance) -> AdjList
add1ToAdjList [] (city1,city2,distance) = [(city1,[(city2,distance)])]
add1ToAdjList ((c1,c_adj):xs) (city1,city2,distance) = if c1 == city1 then (c1, c_adj ++ [(city2,distance)]):xs else (c1,c_adj) : add1ToAdjList xs (city1, city2, distance)



--Get a list with all cities from a RoadMap
--Joins every reference of any city in a list and removes duplicates 
cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

--Return true if city1 and city2 have a direct edge and false otherwise
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(n1, n2, _) -> (n1 == city1 && n2 == city2) || (n1 == city2 && n2 == city1)) roadMap

--From an RoadMap returns the distance of the edge that connects city1 and city2 and Nothing if it doesn't exist
--(c1,c2,dist) is the first edge of the roadMap and xs the other edges 
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, dist):xs) city1 city2 = if ((c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) then Just dist else distance xs city1 city2

--From an AdjList returns the distance of the edge that connects c1 and c2 and Nothing if it doesn't exist
distanceAdjList :: AdjList -> City -> City -> Maybe Distance
distanceAdjList [] _ _ = Nothing
distanceAdjList adjList c1 c2 = lookup c2 edges
    where
        edges = case lookup c1 adjList of
                    Just res -> res
                    Nothing -> []
                                
--From an RoadMap return the Edges of Vertex origin
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap origin = [(city, dist) | (city1,city2,dist) <- roadMap, city <- if origin == city1  then [city2] else if origin == city2 then [city1] else [] ]

--From an AdjList return the Edges of Vertex origin
adjacentAdjList :: AdjList -> City -> [(City,Distance)]
adjacentAdjList adjList origin = case lookup origin adjList of
                                    Just neighbors -> neighbors
                                    Nothing -> []

--From an AdjList return the cities with a direct edge to/from "city"
getNeighbors :: AdjList -> City -> [City]
getNeighbors adjList city = case lookup city adjList of
                                Just neighbors -> map fst neighbors
                                Nothing -> []

--Return the sum of the distances in a path
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ []  = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (city1:city2:rest) =
    case distance roadMap city1 city2 of
        Just dist -> case pathDistance roadMap (city2:rest) of
                        Just dist2 -> Just (dist + dist2)
                        Nothing -> Nothing
        Nothing -> Nothing

--Return the cities with the most Edges
rome :: RoadMap -> [City]
rome roadMap = [city | (city, degree) <- cityDeegreList, degree == max] 
  where
    allRoads = [city | (city1, city2, _) <- roadMap, city <- [city1, city2]] 
    cityGroups = Data.List.group (Data.List.sort allRoads)
    cityDeegreList = [(head cities, length cities) | cities <- cityGroups]
    max = maximum [degree | (_, degree) <- cityDeegreList]


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
        bfsAux :: [(Distance, Path)] -> [Path] -> Distance -> [Path]
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
travelSales roadMap
    | roadMap == [] = []
    | otherwise = undefined



            
{-
travelSales roadMap = snd (minPath [dp (citiesMask, end, [origin]) | end <- cityIndicies, end /= originIndex])
    where
        origin = case roadMap of
                    ((firstCity,_,_) : _) -> firstCity
                    [] -> error "There are no edges" 
        adjList = buildAdjList roadMap
        citiesList = cities roadMap
        numCities = length adjList
        cityIndicies = [0..numCities-1]
        cityMap = zip citiesList cityIndicies
        cityLookup = zip cityIndicies citiesList
        citiesMask = (Data.Bits.shiftL 1 numCities) - 1
        maxInt = maxBound :: Int

        -- Convert a city to an index
        cityToIndex :: City -> Int
        cityToIndex city = case lookup city cityMap of
                                Just res -> res
                                Nothing  -> error "Tried to find a city that does not exist"


        -- Convert an index to a city
        indexToCity :: Int -> City
        indexToCity index = case lookup index cityLookup of
                                Just res -> res
                                Nothing  -> error "Tried to find a Index that does not exist"

        originIndex = cityToIndex origin


        dp :: (Int, Int, Path) -> (Distance, Path)
        dp (mask, end, path)
            | mask == Data.Bits.shiftL 1 end = case distanceAdjList adjList origin (indexToCity end) of 
                                                    Just dist -> (dist,path++[indexToCity end])
                                                    Nothing -> (maxInt,[])             
            | otherwise = minPath
                [let (distPrev, pathPrev) in]
                
                [(dp (Data.Bits.clearBit mask end, prev, path ++ [indexToCity end])) 
                | prev <- cityIndicies,
                    Data.Bits.testBit mask prev,
                    distanceAdjList adjList (indexToCity prev) (indexToCity end) /= Nothing]

        minPath :: [(Distance, Path)] -> (Distance, Path)
        minPath paths = foldl (\(dist1,path1) (dist2,path2)-> if dist1 < dist2 then (dist1,path1) else (dist2,path2)) (head paths) (tail paths)
-}

{-
travelSales :: RoadMap -> Path
travelSales roadMap = tspAux adjList [(0,[origin])] maxInt numCities []
    where
        adjList = buildAdjList roadMap 
        origin = case roadMap of
                    ((firstCity,_,_) : _) -> firstCity
                    [] -> error "There are no edges"
        maxInt = maxBound :: Int
        numCities = length adjList

        tspAux :: AdjList -> [(Distance,Path)] -> Distance -> Int -> Path -> Path
        tspAux _ [] _ _ bestPath = bestPath
        tspAux adjList ((currDist, currPath@(currCity : currVisited)) : queue) bestDist numCities bestPath
            | numCities == length currPath =
                case lookup origin (adjacentAdjList adjList currCity) of
                    Just returnDist -> if (currDist + returnDist) < bestDist 
                                            then tspAux adjList queue (currDist + returnDist) numCities (origin : currPath)     -- Found a smaller TSP path
                                            else tspAux adjList queue bestDist numCities bestPath                               -- The return edge makes the path too long
                    Nothing -> tspAux adjList queue bestDist numCities bestPath                                                 -- No edge to return to the first edge
            | otherwise = tspAux adjList (queue ++ newPaths) bestDist numCities bestPath  
                where 
                    neighbors = adjacentAdjList adjList currCity
                    newPaths = [(currDist + dist, nextCity : currPath) | (nextCity,dist) <- neighbors, not (elem nextCity currPath), currDist + dist <= bestDist]
-}
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