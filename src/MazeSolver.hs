module MazeSolver (solveMaze) where

import DataStructures (Position, Path, Maze, GeneratedMaze(..))

-- Solve a maze given a start position
-- Returns the path from start to end, or empty list if no solution
-- ENTRY POINT DO NOT DELETE
solveMaze :: GeneratedMaze -> Path
solveMaze (GeneratedMaze s e g) = bfs [[s]] []
  where
    bfs [] _ = []  -- No solution
    bfs (path:paths) visited
        | current == e = reverse path  -- Found the end
        | current `elem` visited = bfs paths visited  -- Already visited
        | otherwise = bfs (paths ++ newPaths) (current:visited)
      where
        current = head path
        neighbors = filter (isValid g) (getNeighbors current)
        newPaths = [n:path | n <- neighbors, n `notElem` visited]

-- Get adjacent positions (up, down, left, right)
getNeighbors :: Position -> [Position]
getNeighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

-- Check if a position is valid and walkable
isValid :: Maze -> Position -> Bool
isValid maze (x,y)
    | y < 0 || y >= length maze = False
    | x < 0 || x >= length (head maze) = False
    | otherwise = maze !! y !! x /= '#'

