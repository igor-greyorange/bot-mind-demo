# Bot Mind - Path Planning Server

A distributed bot coordination system built in Erlang/OTP with intelligent path planning capabilities.

## 🌟 Features

- **9×9 Grid System** with configurable obstacles
- **A* Path Planning Algorithm** for optimal route calculation
- **Multi-Bot Management** with concurrent bot coordination
- **TCP Communication** for real-time bot commands
- **Mnesia Database** for persistent grid state
- **RESTful-style Commands** for easy integration

## 🚀 Quick Start

### Prerequisites
- Erlang/OTP 27+ 
- Rebar3

### Installation & Setup
```bash
# Clone and navigate to project
cd bot_mind

# Build the project
make build

# Run tests
make test

# Start the server
make run
```

## 📋 Makefile Commands

### Build Commands
```bash
make build        # Compile the application
make clean        # Clean build artifacts
make deps         # Get dependencies
```

### Run Commands
```bash
make run          # Start the server on port 5555
make server       # Alias for run
make shell        # Start Erlang shell with app loaded
```

### Test Commands
```bash
make test         # Run all tests
make test-unit    # Run unit tests
make test-demo    # Run integration demo
make test-grid    # Test grid functionality
make test-path    # Test path planning
make test-bot     # Test bot client connection
```

### Development Commands
```bash
make dev          # Full dev cycle (clean, compile, test)
make quick        # Quick compile and basic test
make check        # Check for compilation errors
make status       # Check if server is running
make stop         # Stop any running servers
```

## 🎮 Bot Commands

Connect to the server on port 5555 and use these commands:

### Basic Commands
```
ping                         # Test connection
get_empty_positions          # Get all available grid positions
```

### Path Planning
```
plan_path 1,1 5,5           # Plan path between coordinates
```

### Bot Management  
```
register_bot bot1 3,4       # Register a new bot at position
move_bot bot1 5,7           # Update bot position
move_to bot1 8,8            # Move bot to destination (auto-calculates path)
assign_path bot1 1,1 9,9    # Assign specific source-destination path
get_bot bot1                # Get bot information
get_all_bots                # Get all registered bots
```

## 🗺️ Grid Layout

The system uses a 9×9 coordinate grid:

```
  1 2 3 4 5 6 7 8 9
1 . . . . . . . . .
2 . . X . . . . . .    X = Obstacles at (2,3), (5,5), (7,2)
3 . . . . . . . . .    . = Empty positions (78 total)
4 . . . . . . . . .
5 . . . . X . . . .
6 . . . . . . . . .
7 . X . . . . . . .
8 . . . . . . . . .
9 . . . . . . . . .
```

## 🏗️ Architecture

```
bot_mind/
├── src/                    # Application source code
│   ├── bot_mind_app.erl   # OTP application
│   ├── bot_mind_sup.erl   # Supervisor
│   ├── bot_comm_listener.erl # TCP server
│   ├── bot_session.erl    # Client session handler
│   ├── bot_manager.erl    # Bot state management
│   ├── grid_manager.erl   # Grid setup and queries
│   ├── path_planner.erl   # A* pathfinding algorithm
│   └── grid_manager.hrl   # Grid record definitions
├── tests/                 # Test scripts
├── scripts/               # Utility scripts
├── docs/                  # Documentation
└── Makefile              # Build automation
```

## 🤖 Example Usage

### 1. Start Server
```bash
make run
# Server starts on port 5555
```

### 2. Connect Bot
```bash
telnet localhost 5555
```

### 3. Use Bot Commands
```
> register_bot mybot 1,1
OK: Bot registered

> move_to mybot 9,9  
OK: Bot moving to (9,9) via [(1,1),(2,1),(3,1),...,(9,9)]

> get_empty_positions
[(1,1),(1,2),(1,3),...] # 78 empty positions
```

## 🧪 Testing

The project includes comprehensive tests:

- **Unit Tests**: Grid functionality, path planning algorithms
- **Integration Tests**: Complete system workflow
- **Demo Tests**: Interactive demonstrations
- **Client Tests**: Bot connection and communication

```bash
# Run specific test categories
make test-grid    # Grid system tests
make test-path    # Path planning tests  
make test-bot     # Bot client tests
make test-demo    # Full system demo
```

## 🔧 Configuration

### Grid Obstacles
Edit `src/grid_manager.erl` to change obstacle positions:
```erlang
Obstacles = [{2,3}, {5,5}, {7,2}],  % Modify these coordinates
```

### Server Port
Edit `src/bot_mind_app.erl` to change server port:
```erlang
Port = 5555,  % Change this port number
```

## 📡 API Reference

### Path Planning API
- **Input**: Source and destination coordinates
- **Output**: Optimal path as list of coordinates
- **Algorithm**: A* with Manhattan distance heuristic

### Bot Management API
- **Registration**: Associate bot ID with position and process
- **Movement**: Update bot positions with automatic path planning
- **Querying**: Get bot status and positions

## 🚦 Development

### Adding New Features
1. Implement in appropriate module (`src/`)
2. Add tests in `tests/`
3. Update documentation
4. Run `make dev` to test

### Code Organization
- **OTP Principles**: Proper supervision trees
- **Modular Design**: Separated concerns
- **Error Handling**: Graceful failure recovery
- **Testing**: Comprehensive test coverage

## 📜 License

See `LICENSE.md` for license information.

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Add tests for new functionality
4. Run `make test` to ensure all tests pass
5. Submit pull request

---

**Built with ❤️ in Erlang/OTP**
