# Bot Mind - Project Structure Documentation

## 📁 Directory Structure

```
bot_mind/
├── src/                          # Source code
│   ├── bot_mind_app.erl         # Application entry point
│   ├── bot_mind_sup.erl         # OTP supervisor
│   ├── bot_comm_listener.erl    # TCP connection handler
│   ├── bot_session.erl          # Individual bot session handler
│   ├── bot_manager.erl          # Bot registration & management
│   ├── path_planner.erl         # A* pathfinding algorithm
│   ├── grid_manager.erl         # 9x9 grid management
│   ├── grid_manager.hrl         # Record definitions
│   └── bot_mind.app.src         # Application metadata
│
├── tests/                       # Test suite (organized by category)
│   ├── unit/                    # Unit tests
│   │   └── test_app.escript     # Application startup test
│   ├── integration/             # Integration tests
│   │   ├── test_mnesia_system.escript    # Mnesia bot storage test
│   │   ├── test_path_planning.escript    # Path planning test
│   │   ├── test_bot_client.escript       # Bot client connection test
│   │   └── final_demo.escript            # Complete system demo
│   └── debug/                   # Debug utilities
│       ├── debug_grid.escript            # Grid debugging
│       └── detailed_debug.escript       # Detailed system debug
│
├── scripts/                     # Utility scripts
│   ├── start_server.escript     # Server startup script
│   ├── stop_server.escript      # Server shutdown script
│   └── server_status.escript    # Server status checker
│
├── docs/                        # Documentation
│   └── STRUCTURE.md             # This file
│
├── _build/                      # Build artifacts (auto-generated)
├── Makefile                     # Build automation
├── rebar.config                 # Build configuration
├── rebar.lock                   # Dependency lock file
└── README.md                    # Project overview
```

## 🧩 Component Overview

### Core Components

**Application Layer:**
- `bot_mind_app.erl` - OTP application behavior, starts Mnesia and supervisor
- `bot_mind_sup.erl` - OTP supervisor managing worker processes

**Communication Layer:**
- `bot_comm_listener.erl` - TCP server listening on port 5555
- `bot_session.erl` - Per-connection session handling bot commands

**Business Logic:**
- `bot_manager.erl` - Bot registration, movement, and state management in Mnesia
- `path_planner.erl` - A* algorithm for optimal pathfinding
- `grid_manager.erl` - 9x9 grid with obstacles, stored in Mnesia

### Data Storage

**Mnesia Tables:**
- `cell` table - Grid positions: `{key={X,Y}, type=empty|obstacle}`
- `bot` table - Bot records: `{id, session_pid, position, status, task, path}`

### Test Organization

**Unit Tests (`tests/unit/`):**
- Basic application functionality
- Individual component testing

**Integration Tests (`tests/integration/`):**
- End-to-end system testing
- Multi-component interaction testing
- Network communication testing

**Debug Tools (`tests/debug/`):**
- Grid state inspection
- System debugging utilities

## 🛠 Build Commands

### Basic Operations
```bash
make build      # Compile the project
make clean      # Clean build artifacts  
make run        # Start the server
make shell      # Start Erlang shell
```

### Testing
```bash
make test           # Run all tests
make test-unit      # Run unit tests only
make test-integration # Run integration tests only
make test-mnesia    # Test Mnesia bot storage
make test-path      # Test path planning
make test-grid      # Test grid functionality
make quick          # Quick demo test
```

### Server Management
```bash
make status     # Check server status
make stop       # Stop running server
```

### Development
```bash
make dev        # Full development cycle (clean, compile, test)
make help       # Show all available commands
```

## 🚀 Usage Examples

### Starting the System
```bash
# Method 1: Using Makefile
make run

# Method 2: Direct script execution
./scripts/start_server.escript
```

### Testing Bot Operations
```bash
# Connect to server
telnet localhost 5555

# Available commands:
ping                        # Test connection
get_empty_positions         # List available positions
register_bot bot1 3,4       # Register new bot
move_bot bot1 7,8          # Move bot to position
plan_path 1,1 9,9          # Calculate optimal path
get_bot bot1               # Get bot details
get_all_bots               # List all registered bots
```

### Running Automated Tests
```bash
# Comprehensive system test
make test-mnesia

# Path planning verification  
make test-path

# Quick functionality check
make quick
```

## 🎯 Key Features

- **Persistent Storage**: All bot data stored in Mnesia database
- **Real-time Communication**: TCP server handles multiple concurrent bots
- **Optimal Pathfinding**: A* algorithm avoids obstacles efficiently
- **Transaction Safety**: ACID properties for concurrent bot operations
- **Professional Structure**: Organized codebase with proper separation of concerns
- **Comprehensive Testing**: Unit, integration, and debug test suites
- **Build Automation**: Complete Makefile with colored output and error handling

## 📝 Development Guidelines

1. **Code Organization**: Keep source in `src/`, tests in `tests/`, utilities in `scripts/`
2. **Testing**: Always test changes with appropriate test category
3. **Documentation**: Update this file when adding new components
4. **Build Process**: Use Makefile commands for consistent builds
5. **Error Handling**: Follow Erlang OTP error handling patterns