{-# LANGUAGE OverloadedStrings #-}

module WebServer (runWebServer) where

import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Text.Read (readMaybe)

import MazeGenerator (generateMaze)
import MazeSolver (solveMaze)
import API.Types (toMazeResponse, toSolveResponse)

-- | Default port for the web server
defaultPort :: Int
defaultPort = 3000

-- | Run the web server
runWebServer :: IO ()
runWebServer = do
    putStrLn $ "🚀 MazeCraft Web Server starting on http://localhost:" ++ show defaultPort
    putStrLn "   Press Ctrl+C to stop the server"
    scotty defaultPort app

-- | Main Scotty application
app :: ScottyM ()
app = do
    -- Enable CORS for development
    middleware simpleCors
    
    -- Serve static files from src/frontend
    middleware $ staticPolicy (addBase "src/frontend")
    
    -- Root route - serve index.html
    get "/" $ file "src/frontend/index.html"
    
    -- API: Generate a new maze
    -- GET /api/generate?width=20&height=15
    get "/api/generate" $ do
        widthParam <- queryParamMaybe "width"
        heightParam <- queryParamMaybe "height"
        
        let width = ensureOdd $ clamp 5 51 $ fromMaybe 21 (widthParam >>= readMaybe . TL.unpack)
        let height = ensureOdd $ clamp 5 51 $ fromMaybe 15 (heightParam >>= readMaybe . TL.unpack)
        
        maze <- liftIO $ generateMaze width height
        json $ toMazeResponse maze
    
    -- API: Generate and solve a maze in one call
    -- GET /api/generate-solved?width=20&height=15
    get "/api/generate-solved" $ do
        widthParam <- queryParamMaybe "width"
        heightParam <- queryParamMaybe "height"
        
        let width = ensureOdd $ clamp 5 51 $ fromMaybe 21 (widthParam >>= readMaybe . TL.unpack)
        let height = ensureOdd $ clamp 5 51 $ fromMaybe 15 (heightParam >>= readMaybe . TL.unpack)
        
        maze <- liftIO $ generateMaze width height
        let path = solveMaze maze
        
        json $ object 
            [ "maze" .= toMazeResponse maze
            , "solution" .= toSolveResponse path
            ]
    
    -- API: Health check
    get "/api/health" $ do
        json $ object ["status" .= ("ok" :: String), "service" .= ("MazeCraft" :: String)]

-- | Ensure a number is odd (required for maze generation)
ensureOdd :: Int -> Int
ensureOdd n
    | odd n     = n
    | otherwise = n + 1

-- | Clamp a value between min and max
clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal val = max minVal (min maxVal val)
