module MazeGenerator (generateMaze) where

import DataStructures (GeneratedMaze(..), Position)
import System.Random (newStdGen, randomR, StdGen)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

-- Generate a perfect maze with given dimensions using Recursive Backtracking
-- A perfect maze has exactly one path between any two points (no loops, no isolated areas)
-- '#' represents walls, ' ' represents paths
--ENTRY POINT DONT DELETE
generateMaze :: Int -> Int -> IO GeneratedMaze
generateMaze width height
    | width < 5 || height < 5 = return $ GeneratedMaze (-1,-1) (-1,-1) []
    | otherwise = do
        -- Get a fresh random generator for each maze
        gen <- newStdGen
        
        -- Start and end positions (must be on odd coordinates for the algorithm)
        let startPos = (1, 1)
        let endPos = (adjustToOdd (width - 2), adjustToOdd (height - 2))
        
        -- Generate the maze grid
        let mazeGrid = generatePerfectMaze width height gen
        
        return $ GeneratedMaze startPos endPos mazeGrid

-- Ensure coordinate is odd (required for maze cell positions)
adjustToOdd :: Int -> Int
adjustToOdd n
    | odd n     = n
    | otherwise = n - 1

-- Generate a perfect maze using Recursive Backtracking (Depth-First Search)
generatePerfectMaze :: Int -> Int -> StdGen -> [[Char]]
generatePerfectMaze w h gen = arrayToGrid w h visitedCells
  where
    -- Start from position (1,1) and carve passages
    startCell = (1, 1)
    visitedCells = carvePassages startCell (Set.singleton startCell) gen

    -- Recursive backtracking: carve passages through the maze
    carvePassages :: Position -> Set.Set Position -> StdGen -> Set.Set Position
    carvePassages pos visited gen0 = 
        foldl processNeighbor visited shuffledNeighbors
      where
        -- Get all valid neighboring cells (2 steps away in cardinal directions)
        neighbors = getUnvisitedNeighbors pos visited w h
        -- Shuffle neighbors for randomness
        shuffledNeighbors = shuffleList neighbors gen0
        
        -- Process each neighbor: carve wall between current and neighbor, then recurse
        processNeighbor :: Set.Set Position -> (Position, StdGen) -> Set.Set Position
        processNeighbor vis (neighbor, nextGen)
            | Set.member neighbor vis = vis  -- Already visited, skip
            | otherwise = carvePassages neighbor newVisited nextGen
          where
            -- The wall between current cell and neighbor
            wallPos = ((fst pos + fst neighbor) `div` 2, (snd pos + snd neighbor) `div` 2)
            -- Mark both the wall and the neighbor as passages
            newVisited = Set.insert neighbor (Set.insert wallPos vis)

-- Get unvisited neighbors (cells that are 2 steps away in cardinal directions)
getUnvisitedNeighbors :: Position -> Set.Set Position -> Int -> Int -> [Position]
getUnvisitedNeighbors (x, y) visited w h = 
    filter isValidUnvisited [(x-2, y), (x+2, y), (x, y-2), (x, y+2)]
  where
    isValidUnvisited (nx, ny) = 
        nx > 0 && nx < w - 1 && 
        ny > 0 && ny < h - 1 && 
        not (Set.member (nx, ny) visited)

-- Shuffle a list using a random generator, pairing each element with a new generator
shuffleList :: [Position] -> StdGen -> [(Position, StdGen)]
shuffleList xs gen = zip sortedXs gens
  where
    -- Generate random values for sorting
    (randomVals, gens) = generateRandoms (length xs) gen
    -- Pair elements with random values and sort
    paired = zip xs randomVals
    sortedPaired = sortBy (comparing snd) paired
    sortedXs = map fst sortedPaired

-- Generate a list of random integers and corresponding generators
generateRandoms :: Int -> StdGen -> ([Int], [StdGen])
generateRandoms 0 _ = ([], [])
generateRandoms n gen = (val : vals, gen' : gens)
  where
    (val, gen') = randomR (0, 1000000) gen
    (vals, gens) = generateRandoms (n - 1) gen'

-- Convert the set of passage cells to a 2D grid
arrayToGrid :: Int -> Int -> Set.Set Position -> [[Char]]
arrayToGrid w h passages = [[cellAt x y | x <- [0..w-1]] | y <- [0..h-1]]
  where
    cellAt x y
        | x == 0 || x == w - 1 = '#'  -- Left and right borders
        | y == 0 || y == h - 1 = '#'  -- Top and bottom borders
        | Set.member (x, y) passages = ' '  -- Passage
        | otherwise = '#'  -- Wall
