module UI (runUI) where

import MazeGenerator (generateMaze)
import MazeSolver (solveMaze)
import DataStructures (GeneratedMaze(..), Position)

-- Main UI entry point
-- ENTRY POINT DON'T DELETE
runUI :: IO ()
runUI = do
    let maze = generateMaze 10 8
    let path = solveMaze maze
    putStrLn "\nGenerated Maze:"
    displayMaze maze []
    putStrLn "\nSolved Maze (path marked with *):"
    displayMaze maze path
    putStrLn $ "\nPath length: " ++ show (length path)

-- Display the maze with optional path markers
displayMaze :: GeneratedMaze -> [Position] -> IO ()
displayMaze (GeneratedMaze s e g) path = mapM_ putStrLn (markPath g path s e)

-- Mark the path on the maze grid
markPath :: [[Char]] -> [Position] -> Position -> Position -> [String]
markPath maze path start end = [[getChar x y | x <- [0..width-1]] | y <- [0..height-1]]
  where
    height = length maze
    width = if height > 0 then length (head maze) else 0
    getChar x y
        | (x,y) == start = 'S'
        | (x,y) == end = 'E'
        | (x,y) `elem` path = '*'
        | otherwise = maze !! y !! x

