module MazeSolver (solveMaze) where

import DataStructures (Position, Path, Maze, GeneratedMaze(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.List (foldl')

-- ENTRY POINT DO NOT DELETE
-- Solve the maze using BFS. Uses:
--  - `Seq` as a queue (O(1) push/pop),
--  - `Set` for visited checks (O(log n) or better),
--  - `Map` to store parent pointers for path reconstruction.
solveMaze :: GeneratedMaze -> Path
solveMaze (GeneratedMaze s e g) =
    let parents = bfs (Seq.singleton s) (Set.singleton s) Map.empty
    in reconstructPath s e parents
  where
    bfs :: Seq.Seq Position -> Set.Set Position -> Map.Map Position Position -> Map.Map Position Position
    bfs Seq.Empty _ parents = parents
    bfs (cur Seq.:<| q) visited parents
        | cur == e = parents
        | otherwise =
            let neighbors = filter (isValid g) (getNeighbors cur)
                -- Only consider neighbors not yet visited
                unvisited = filter (`Set.notMember` visited) neighbors
                -- Mark them visited immediately to avoid duplicates in the queue
                visited' = foldl' (flip Set.insert) visited unvisited
                -- Enqueue unvisited neighbors
                q' = q Seq.>< Seq.fromList unvisited
                -- Record parent pointers: parent[neighbor] = cur
                parents' = foldl' (\m v -> Map.insert v cur m) parents unvisited
            in bfs q' visited' parents'

    -- Walk parents from goal back to start, building the path in forward order.
    reconstructPath :: Position -> Position -> Map.Map Position Position -> Path
    reconstructPath src dst parents
        | src == dst = [src]
        | otherwise = case go dst [] of
            Nothing -> []
            Just p -> p
      where
        go :: Position -> [Position] -> Maybe Path
        go cur acc
            | cur == src = Just (src : acc)
            | otherwise = case Map.lookup cur parents of
                Nothing -> Nothing
                Just p -> go p (cur : acc)

-- Neighbors (left, right, up, down)
getNeighbors :: Position -> [Position]
getNeighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

-- Bounds and wall check.
isValid :: Maze -> Position -> Bool
isValid maze (x,y) =
    let rows = length maze
    in if rows == 0 then False
       else
           let cols = length (head maze)
           in if y < 0 || y >= rows || x < 0 || x >= cols
              then False
              else (maze !! y !! x) /= '#'

