{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import DataStructures (Position, GeneratedMaze(..))

-- | Response for maze generation
data MazeResponse = MazeResponse
    { mazeGrid   :: [[Char]]  -- The maze grid
    , mazeWidth  :: Int       -- Width of the maze
    , mazeHeight :: Int       -- Height of the maze
    , startPos   :: Position  -- Start position (x, y)
    , endPos     :: Position  -- End position (x, y)
    } deriving (Show, Generic)

instance ToJSON MazeResponse where
    toJSON (MazeResponse grid w h s e) = object
        [ "grid"   .= grid
        , "width"  .= w
        , "height" .= h
        , "start"  .= object ["x" .= fst s, "y" .= snd s]
        , "end"    .= object ["x" .= fst e, "y" .= snd e]
        ]

-- | Response for maze solving
data SolveResponse = SolveResponse
    { path       :: [Position]  -- The solution path
    , pathLength :: Int         -- Length of the path
    , status     :: String      -- "solved" or "unsolvable"
    } deriving (Show, Generic)

instance ToJSON SolveResponse where
    toJSON (SolveResponse p len st) = object
        [ "path"       .= map (\(x, y) -> object ["x" .= x, "y" .= y]) p
        , "pathLength" .= len
        , "status"     .= st
        ]

-- | Convert a GeneratedMaze to MazeResponse
toMazeResponse :: GeneratedMaze -> MazeResponse
toMazeResponse (GeneratedMaze s e g) = MazeResponse
    { mazeGrid   = g
    , mazeWidth  = if null g then 0 else length (head g)
    , mazeHeight = length g
    , startPos   = s
    , endPos     = e
    }

-- | Create a SolveResponse from a path
toSolveResponse :: [Position] -> SolveResponse
toSolveResponse p
    | null p    = SolveResponse p 0 "unsolvable"
    | otherwise = SolveResponse p (length p) "solved"
