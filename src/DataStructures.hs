module DataStructures where

-- Data structures used across the MazeCraft project

-- Maze representation as a grid of characters
type Maze = [[Char]]

-- Position in the maze represented as (x, y) coordinates
type Position = (Int, Int)

-- Path through the maze represented as a list of positions
type Path = [Position]

data GeneratedMaze = GeneratedMaze {start::Position,end::Position,grid::Maze}
    deriving (Show)
