# MazeCraft

A maze generation and solving application written in Haskell. MazeCraft generates mazes and finds optimal paths through them using breadth-first search (BFS) algorithm.

## Features

- **Maze Generation**: Creates configurable mazes with customizable dimensions
- **Pathfinding**: Solves mazes using BFS algorithm to find the shortest path
- **Visual Display**: Displays mazes in ASCII art with clear path visualization
- **Path Markers**: 
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

After building, run the executable:

```bash
cabal run MazeCraft
```

Alternatively, you can run the built executable directly:
```bash
./dist-newstyle/build/x86_64-windows/ghc-9.6.7/MazeCraft-0.1.0.0/x/MazeCraft/build/MazeCraft/MazeCraft.exe
```

## Example Output

```
Welcome to MazeCraft!

Generated Maze:
##########
#        #
#        #
#        #
#        #
#        #
#        #
##########

Solved Maze (path marked with *):
##########
#S*******#
#       *#
#       *#
#       *#
#       *#
#       *#
#######E#

Path length: 14
```

## Project Structure

```
MazeCraft/
├── src/
│   ├── Main.hs           # Application entry point
│   ├── UI.hs             # User interface and display logic
│   ├── MazeGenerator.hs  # Maze generation algorithms
│   ├── MazeSolver.hs     # Pathfinding algorithms (BFS)
│   └── DataStructures.hs # Core data type definitions
├── MazeCraft.cabal       # Build configuration
└── README.md             # This file
```

## Module Overview

- **Main**: Entry point that initializes the application
- **UI**: Handles maze display and user interface
- **MazeGenerator**: Generates mazes with specified dimensions
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
