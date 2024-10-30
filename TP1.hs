import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import qualified Data.Map
import Data.Maybe (mapMaybe)
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(n1, n2, _) -> (n1 == city1 && n2 == city2) || (n1 == city2 && n2 == city1)) roadMap

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, dist):xs) city1 city2 = if ((c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) then Just dist else distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap origin = [(city, dist) | (city1,city2,dist) <- roadMap, city <- if origin == city1  then [city2] else if origin == city2 then [city1] else [] ]

adjacentAux :: RoadMap -> City -> [(City,Distance)]
adjacentAux = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (city1:city2:rest)
    | areAdjacent roadMap city1 city2 =
      case distance roadMap city1 city2 of
        Just dist -> case pathDistance roadMap (city2:rest) of
            Just distRest -> Just (dist + distRest)
            Nothing -> Nothing
        Nothing -> Nothing 
    | otherwise = Nothing


rome :: RoadMap -> [City]
rome roadMap = [city | (city, degree) <- cityDeegreList, degree == max] 
  where
    allRoads = [city | (city1, city2, _) <- roadMap, city <- [city1, city2]] 
    cityGroups = Data.List.group (Data.List.sort allRoads)
    cityDeegreList = [(head cities, length cities) | cities <- cityGroups]
    max = maximum [degree | (_, degree) <- cityDeegreList]


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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