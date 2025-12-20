module Main where

import UI (runUI)

main :: IO ()
main = do
    putStrLn "Welcome to MazeCraft!"
    runUI
