#!/bin/bash
# MazeCraft Launcher Script

echo "🎮 MazeCraft - Maze Generator"
echo "=============================="
echo ""
echo "Select mode:"
echo "  1) Web Interface (default) - http://localhost:3000"
echo "  2) Terminal (CLI)"
echo ""
read -p "Enter choice [1-2] (default: 1): " choice

case $choice in
    2)
        echo "Starting Terminal mode..."
        cabal run MazeCraft -- cli
        ;;
    *)
        echo "Starting Web Server..."
        echo "Open http://localhost:3000 in your browser"
        echo "Press Ctrl+C to stop"
        cabal run MazeCraft -- web
        ;;
esac
