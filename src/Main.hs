module Main where

import System.Environment (getArgs)
import UI (runUI)
import WebServer (runWebServer)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["web"]  -> runWebServer
        ["cli"]  -> runCLI
        []       -> runWebServer  -- Default to web mode
        _        -> printUsage

runCLI :: IO ()
runCLI = do
    putStrLn "Welcome to MazeCraft!"
    runUI

printUsage :: IO ()
printUsage = do
    putStrLn "MazeCraft - Maze Generation and Solving"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  MazeCraft        Start web server (default)"
    putStrLn "  MazeCraft web    Start web server"
    putStrLn "  MazeCraft cli    Run in terminal mode"
