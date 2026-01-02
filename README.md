# MazeCraft

A maze generation and solving application written in Haskell. MazeCraft generates mazes and finds optimal paths through them using breadth-first search (BFS) algorithm.

## Features

- **🎨 Web Interface**: Modern, responsive web UI with dark/light theme
- **🔀 Random Maze Generation**: Creates unique mazes every time using Recursive Backtracking
- **🎯 Pathfinding**: Solves mazes using BFS algorithm to find the shortest path
- **📊 Multiple Complexity Levels**: 5 difficulty levels from simple (11x9) to complex (51x35)
- **👁️ Solution Toggle**: Show/hide the solution path interactively
- **💻 CLI Mode**: Original terminal interface with ASCII art visualization
- **🌐 REST API**: JSON endpoints for maze generation and solving

### Path Markers (CLI Mode)
  - `S` - Start position
  - `E` - End position
  - `*` - Solution path
  - `#` - Walls
  - ` ` (space) - Open walkable paths

## Prerequisites

To build and run MazeCraft, you need:

- **GHC (Glasgow Haskell Compiler)** - Version 9.6.7 or compatible
- **Cabal** - Haskell build tool (version 2.4 or higher)

### Installing GHC and Cabal

**Windows:**
- Install [GHCup](https://www.haskell.org/ghcup/) which provides GHC and Cabal
- Or download from [Haskell Platform](https://www.haskell.org/platform/)

**Linux/macOS:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Building the Project

1. Clone or navigate to the project directory:
```bash
cd MazeCraft
```

2. Build the project using Cabal:
```bash
cabal build
```

This will compile all source files and create an executable.

## Running MazeCraft

### Web Interface (Default)

After building, start the web server:

```bash
cabal run MazeCraft
```

Or explicitly:

```bash
cabal run MazeCraft -- web
```

Then open your browser at `http://localhost:3000`

The web interface features:
- **Complexity Slider**: Adjust maze size from simple (11x9) to complex (51x35)
- **Generate Button**: Create a new random maze
- **Solution Toggle**: Show or hide the solution path
- **Theme Toggle**: Switch between dark and light modes
- **Responsive Design**: Works on desktop and mobile devices

### Terminal Mode (CLI)

Run the original ASCII art interface:

```bash
cabal run MazeCraft -- cli
```

## API Endpoints

The web server exposes the following REST API endpoints:

### Generate Maze

```http
GET /api/generate?width=21&height=15
```

Returns a new random maze in JSON format.

### Generate and Solve

```http
GET /api/generate-solved?width=21&height=15
```

Returns a maze with its solution in JSON format.

### Health Check

```http
GET /api/health
```

Returns server status.

## Example CLI Output

```text
Welcome to MazeCraft!

Generated Maze:
##########
#        #
#        #
#        #
##########

Solved Maze (path marked with *):
##########
#S*******#
#       *#
#######E#

Path length: 14
```

## Project Structure

```text
MazeCraft/
├── src/
│   ├── Main.hs           # Application entry point with CLI/Web modes
│   ├── UI.hs             # Terminal UI and ASCII display logic
│   ├── MazeGenerator.hs  # Random maze generation (Recursive Backtracking)
│   ├── MazeSolver.hs     # Pathfinding algorithms (BFS)
│   ├── DataStructures.hs # Core data type definitions
│   ├── WebServer.hs      # Scotty web server with REST API
│   ├── API/
│   │   └── Types.hs      # JSON serialization types
│   └── frontend/
│       ├── index.html    # Modern web interface
│       └── app.js        # Frontend application logic
├── docs/
│   ├── code.html         # Original UI mockup
│   └── screen.png        # Design reference
├── MazeCraft.cabal       # Build configuration with dependencies
└── README.md             # This file
```

## Module Overview

- **Main**: Entry point with CLI/Web mode selection
- **UI**: Terminal interface with ASCII maze rendering
- **MazeGenerator**: Generates unique random mazes using Recursive Backtracking
- **MazeSolver**: BFS pathfinding algorithm for shortest path
- **DataStructures**: Type definitions for mazes and positions
- **WebServer**: Scotty-based web server with REST API
- **API.Types**: JSON serialization for API responses
- **frontend**: Modern web UI with Tailwind CSS
- **MazeSolver**: Implements BFS algorithm to find shortest path
- **DataStructures**: Defines core types (Maze, Position, Path, GeneratedMaze)

## Customization

To modify the maze dimensions, edit the `runUI` function in [src/UI.hs](src/UI.hs):

```haskell
let maze = generateMaze 10 8  -- Change width and height here
```

The first parameter is width, the second is height.

## License

MIT

## Author

Developer (developer@example.com)

## Contributing

Feel free to fork this project and submit pull requests with improvements or new features!
