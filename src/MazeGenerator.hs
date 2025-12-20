module MazeGenerator (generateMaze) where

import DataStructures (GeneratedMaze(..))

-- Generate a simple maze with given dimensions
-- '#' represents walls, ' ' represents paths
--ENTRY POINT DONT DELETE
generateMaze :: Int -> Int -> GeneratedMaze
generateMaze width height
    | width <= 0 || height <= 0 = GeneratedMaze (-1,-1) (-1,-1) []
    | otherwise = GeneratedMaze (1,1) (width-2,height-2) (buildMaze width height)

-- Build a simple maze with borders and a clear path
buildMaze :: Int -> Int -> [[Char]]
buildMaze w h = [makeRow y | y <- [0..h-1]]
  where
    makeRow y
        | y == 0 || y == h-1 = replicate w '#'  -- Top and bottom borders
        | otherwise = '#' : replicate (w-2) ' ' ++ "#"  -- Side borders with open path
