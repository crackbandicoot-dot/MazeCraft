/**
 * MazeCraft - Frontend Application
 * Handles maze generation, rendering, and user interactions
 */

// ============================================================================
// State Management
// ============================================================================

const state = {
    maze: null,          // Current maze data
    solution: null,      // Solution path
    showingSolution: false,
    isLoading: false,
    complexity: 50       // 1-100 scale for fluid adjustment
};

/**
 * Calculate maze dimensions from complexity value (1-100)
 */
function getComplexitySize(complexity) {
    // Linear interpolation from min to max
    const minWidth = 11;
    const maxWidth = 51;
    const minHeight = 9;
    const maxHeight = 35;
    
    // Ensure odd dimensions for the algorithm
    const width = minWidth + Math.floor((complexity - 1) / 99 * (maxWidth - minWidth));
    const height = minHeight + Math.floor((complexity - 1) / 99 * (maxHeight - minHeight));
    
    // Ensure both are odd
    const oddWidth = width % 2 === 0 ? width + 1 : width;
    const oddHeight = height % 2 === 0 ? height + 1 : height;
    
    return { width: oddWidth, height: oddHeight };
}

// ============================================================================
// DOM Elements
// ============================================================================

const elements = {
    mazeContainer: document.getElementById('mazeContainer'),
    generateBtn: document.getElementById('generateBtn'),
    generateBtnText: document.getElementById('generateBtnText'),
    generateIcon: document.getElementById('generateIcon'),
    showSolutionBtn: document.getElementById('showSolutionBtn'),
    solutionBtnText: document.getElementById('solutionBtnText'),
    solutionIcon: document.getElementById('solutionIcon'),
    complexitySlider: document.getElementById('complexitySlider'),
    sizeLabel: document.getElementById('sizeLabel'),
    statusIndicator: document.getElementById('statusIndicator'),
    statusText: document.getElementById('statusText'),
    pathInfo: document.getElementById('pathInfo'),
    themeToggle: document.getElementById('themeToggle'),
    themeIcon: document.getElementById('themeIcon')
};

// ============================================================================
// API Functions
// ============================================================================

/**
 * Generate a new maze from the backend
 */
async function generateMaze() {
    const size = getComplexitySize(state.complexity);
    
    try {
        const response = await fetch(`/api/generate-solved?width=${size.width}&height=${size.height}`);
        if (!response.ok) throw new Error('Failed to generate maze');
        
        const data = await response.json();
        return data;
    } catch (error) {
        console.error('Error generating maze:', error);
        throw error;
    }
}

// ============================================================================
// Rendering Functions
// ============================================================================

/**
 * Render the maze grid
 */
function renderMaze() {
    if (!state.maze) {
        elements.mazeContainer.innerHTML = `
            <div class="flex items-center justify-center h-64 text-[#9dabb9]">
                <span class="text-lg">Click "Generate New Maze" to start</span>
            </div>
        `;
        return;
    }

    const { grid, start, end, width, height } = state.maze;
    const pathSet = new Set();
    
    // Build path set for O(1) lookup
    if (state.showingSolution && state.solution && state.solution.path) {
        state.solution.path.forEach(pos => {
            pathSet.add(`${pos.x},${pos.y}`);
        });
    }

    // Calculate cell size based on grid dimensions
    const cellGap = 2;
    
    // Build grid HTML with fixed gap in pixels
    let gridHTML = `<div class="grid gap-[2px]" style="grid-template-columns: repeat(${width}, 1fr); aspect-ratio: ${width}/${height};">`;
    
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const cell = grid[y][x];
            const isStart = x === start.x && y === start.y;
            const isEnd = x === end.x && y === end.y;
            const isPath = pathSet.has(`${x},${y}`);
            const isWall = cell === '#';
            
            let cellClass = 'maze-cell rounded-sm transition-colors flex items-center justify-center';
            let cellContent = '';
            
            if (isStart) {
                cellClass += ' bg-green-500 shadow-[0_0_12px_rgba(34,197,94,0.6)] z-10 animate-pulse';
                cellContent = '<span class="material-symbols-outlined text-white text-[8px] md:text-[12px] font-bold leading-none">flag</span>';
            } else if (isEnd) {
                cellClass += ' bg-red-500 shadow-[0_0_12px_rgba(239,68,68,0.6)] z-10 animate-pulse';
                cellContent = '<span class="material-symbols-outlined text-white text-[8px] md:text-[12px] font-bold leading-none">sports_score</span>';
            } else if (isPath && state.showingSolution) {
                cellClass += ' bg-primary path-cell';
            } else if (isWall) {
                cellClass += ' bg-slate-300 dark:bg-slate-700';
            } else {
                cellClass += ' bg-white dark:bg-slate-800';
            }
            
            gridHTML += `<div class="${cellClass}">${cellContent}</div>`;
        }
    }
    
    gridHTML += '</div>';
    elements.mazeContainer.innerHTML = gridHTML;
}

/**
 * Update the status display
 */
function updateStatus(status, color = 'yellow') {
    const colorMap = {
        'green': 'bg-green-500',
        'yellow': 'bg-yellow-500',
        'red': 'bg-red-500',
        'blue': 'bg-blue-500'
    };
    
    elements.statusIndicator.className = `size-2 rounded-full ${colorMap[color] || colorMap.yellow}`;
    if (color === 'green') {
        elements.statusIndicator.classList.add('animate-pulse');
    } else {
        elements.statusIndicator.classList.remove('animate-pulse');
    }
    elements.statusText.textContent = `Status: ${status}`;
}

/**
 * Update the size label based on complexity
 */
function updateSizeLabel() {
    const size = getComplexitySize(state.complexity);
    elements.sizeLabel.textContent = `${size.width}x${size.height}`;
}

/**
 * Update path info display
 */
function updatePathInfo() {
    if (state.solution && state.solution.pathLength > 0) {
        elements.pathInfo.textContent = `Path Length: ${state.solution.pathLength}`;
    } else {
        elements.pathInfo.textContent = 'Path Length: --';
    }
}

// ============================================================================
// UI State Functions
// ============================================================================

/**
 * Set loading state
 */
function setLoading(isLoading) {
    state.isLoading = isLoading;
    
    elements.generateBtn.disabled = isLoading;
    elements.showSolutionBtn.disabled = isLoading || !state.maze;
    
    if (isLoading) {
        elements.generateIcon.textContent = '';
        elements.generateIcon.innerHTML = '<div class="spinner"></div>';
        elements.generateBtnText.textContent = 'Generating...';
        updateStatus('Generating...', 'blue');
    } else {
        elements.generateIcon.innerHTML = '';
        elements.generateIcon.textContent = 'refresh';
        elements.generateBtnText.textContent = 'Generate New Maze';
    }
}

/**
 * Update solution button state
 */
function updateSolutionButton() {
    if (state.showingSolution) {
        elements.solutionIcon.textContent = 'visibility_off';
        elements.solutionBtnText.textContent = 'Hide Solution';
    } else {
        elements.solutionIcon.textContent = 'visibility';
        elements.solutionBtnText.textContent = 'Show Solution';
    }
}

// ============================================================================
// Event Handlers
// ============================================================================

/**
 * Handle generate button click
 */
async function handleGenerate() {
    if (state.isLoading) return;
    
    setLoading(true);
    state.showingSolution = false;
    updateSolutionButton();
    
    try {
        const data = await generateMaze();
        state.maze = data.maze;
        state.solution = data.solution;
        
        renderMaze();
        updatePathInfo();
        
        if (state.solution && state.solution.status === 'solved') {
            updateStatus('Solved', 'green');
        } else {
            updateStatus('Generated (No Solution)', 'yellow');
        }
        
        elements.showSolutionBtn.disabled = false;
    } catch (error) {
        updateStatus('Error', 'red');
        console.error('Failed to generate maze:', error);
    } finally {
        setLoading(false);
    }
}

/**
 * Handle show/hide solution button click
 */
function handleToggleSolution() {
    if (!state.maze) return;
    
    state.showingSolution = !state.showingSolution;
    updateSolutionButton();
    renderMaze();
    
    if (state.showingSolution) {
        updateStatus('Showing Solution', 'green');
    } else {
        updateStatus('Solution Hidden', 'yellow');
    }
}

/**
 * Handle complexity slider change
 */
function handleComplexityChange(e) {
    state.complexity = parseInt(e.target.value);
    updateSizeLabel();
}

/**
 * Handle theme toggle
 */
function handleThemeToggle() {
    const html = document.documentElement;
    const isDark = html.classList.contains('dark');
    
    if (isDark) {
        html.classList.remove('dark');
        elements.themeIcon.textContent = 'dark_mode';
        localStorage.setItem('theme', 'light');
    } else {
        html.classList.add('dark');
        elements.themeIcon.textContent = 'light_mode';
        localStorage.setItem('theme', 'dark');
    }
}

/**
 * Initialize theme from localStorage
 */
function initTheme() {
    const savedTheme = localStorage.getItem('theme');
    const html = document.documentElement;
    
    if (savedTheme === 'light') {
        html.classList.remove('dark');
        elements.themeIcon.textContent = 'dark_mode';
    } else {
        html.classList.add('dark');
        elements.themeIcon.textContent = 'light_mode';
    }
}

// ============================================================================
// Initialization
// ============================================================================

function init() {
    // Initialize theme
    initTheme();
    
    // Set initial UI state
    updateSizeLabel();
    elements.showSolutionBtn.disabled = true;
    
    // Attach event listeners
    elements.generateBtn.addEventListener('click', handleGenerate);
    elements.showSolutionBtn.addEventListener('click', handleToggleSolution);
    elements.complexitySlider.addEventListener('input', handleComplexityChange);
    elements.themeToggle.addEventListener('click', handleThemeToggle);
    
    // Generate initial maze
    handleGenerate();
}

// Start the app
document.addEventListener('DOMContentLoaded', init);
