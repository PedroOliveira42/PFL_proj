# PFT First project report

## Function Implementation Distribution
Pedro da Silva Marinho - up202205693 - work done : 40%
    
- cities :: RoadMap -> [City]
- areAdjacent :: RoadMap -> City -> City -> Bool
- pathDistance :: RoadMap -> Path -> Maybe Distance
- rome :: RoadMap -> [City]



Pedro Marta Oliveira - up202206498 - work done : 60%
- distance :: RoadMap -> City -> City -> Maybe Distance
- adjacent :: RoadMap -> City -> [(City,Distance)]
- isStronglyConnected :: RoadMap -> Bool
- shortestPath :: RoadMap -> City -> City -> [Path]
- travelSales :: RoadMap -> Path
- All AdjList related functions


## Shortest Paths

The <code>shortestPath</code> function intends to find all the shortest paths betwen origin city1 and city2.

First we call the function <code>bfs</code> that in turn calls <code>bfsAux</code> that together implement a breath-first-search for the optimal path

bfsAux receives 3 arguments, those being:
    
    - A list filled with tuples, each tuple contains an incomplete solution to the problem and its associated distance 
    - A list with the best paths found yet, since initially no solution was found yet this argument has the value of []
    - The distance associated with the shortest solutions found, since initially no solution was found yet this argument has the value of maxBound :: Int

By using these functions we can find the shortest paths between 2 cities, first we call <code>bfs</code> that will call bfsAux with the list of tuples as "[(0,[city1])]", and then bfsAux will call itsel recursively, in these calls one of 3 things may happen.

### 1- The incomplete path list is empty
In this case we have explored every worthwhile path and can return the best paths found.


### 2- The first incomplete path of the list ends on the destination city
In this case we have found a valid path, if it's the first one we have found or it's shorter than the best one we found yet we update the best distance/Paths list accordingly, if it has the same distance as the best paths found we merely add it to that list, however if it's longer that the best paths found we ignore it and continue.   

### 3- The first incomplete path has not yet arrived at the destination city
In this case we remove the incomplete path from the list and check every neighbour to the last Vertex in the incomplete path, if it hasn't been visited yet and its inclusion would not create a path longer than the best path found yet we append to the list a new incomplete path with that neighbour at the end and the respective distance. 

## Travel Sales

The <code>travelSales</code> function intends to find a solution to the TSP, meaning to find a path that starts and ends on the same vertex, that passes through every other vertex only once and that has the smallest distance possible.

Firstly we convert the graph representation from a RoadMap into a AdjList. Then we call auxiliary function "tspAux".

tspAux receives 4 arguments, those being:
    
    - An AdjList that represents the graph
    - A list filled with tuples, each tuple contains an incomplete solution to the TSP and its associated distance.
    - The distance associated with the shortest solution found, initially since no solution was found yet this argument has the value of maxBound :: Int
    - The Path that is the shortest solution to the TSP found yet, initially since no solution was found yet this argument has the value of []

By using these functions we implement a branch and bound aproach to the TSP problem, first we call tspAux with the list having only the tuple "(0,[originCity])", then we recursively call tspAux, where 3 things may happen. 

### 1- The list is empty
In this case we have explored every worthwhile path and can return the best path found.

### 2- The incomplete path has traversed every Vertex
In this case we check if there is an Edge that connects the last city in the incomplete path to the original city, if there isn't or if there is but the Distance of the looped path would be bigger than the best distance found yet we ignore that solution and continue, however if the distance of the looped path is shorter than the best distance found yet we update the best path/distance accordingly and continue.    

### 3- The incomplete path has not traversed every Vertex yet
In this case we remove the incomplete path from the list and check every neighbour to the last Vertex in the incomplete path, if it hasn't been visited yet and its inclusion would not create a path longer than the best path found yet we append to the list a new incomplete path with that neighbour at the end and the respective distance. 

<b>Note:</b> "originCity" is an arbitrary city since the graph is undirected and therefore if a solution to the TSP exists it can start from any vertex.