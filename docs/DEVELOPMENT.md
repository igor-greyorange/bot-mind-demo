# Development Guide

## Project Structure

```
bot_mind/
├── src/                      # Core application modules
│   ├── bot_mind_app.erl     # OTP application callback
│   ├── bot_mind_sup.erl     # Main supervisor  
│   ├── bot_comm_listener.erl # TCP listener (gen_server)
│   ├── bot_session.erl      # Client session handler (gen_server)
│   ├── bot_manager.erl      # Bot state management
│   ├── grid_manager.erl     # Grid operations and Mnesia interface
│   ├── path_planner.erl     # A* pathfinding algorithm
│   └── grid_manager.hrl     # Record definitions
├── tests/                   # Test scripts and utilities
│   ├── test_app.escript     # Basic application test
│   ├── test_bot_client.escript # Bot client connection test
│   ├── final_demo.escript   # Complete system demo
│   └── ...                  # Other test utilities
├── scripts/                 # Utility scripts
│   └── start_server.escript # Server startup script
├── docs/                    # Documentation
│   └── API.md              # API documentation
├── Makefile                 # Build automation
└── README.md               # Project overview
```

## Module Architecture

### bot_mind_app.erl
- **Type**: OTP Application callback module
- **Purpose**: Application lifecycle management
- **Key Functions**:
  - `start/2` - Initialize Mnesia, setup grid, start supervisor
  - `stop/1` - Graceful shutdown

### bot_mind_sup.erl  
- **Type**: Supervisor
- **Purpose**: Supervise bot_comm_listener process
- **Strategy**: one_for_one restart strategy

### bot_comm_listener.erl
- **Type**: gen_server
- **Purpose**: TCP listener accepting bot connections
- **Key Functions**:
  - `init/1` - Setup TCP listener socket
  - `accept_loop/1` - Accept incoming connections, spawn session handlers

### bot_session.erl
- **Type**: gen_server  
- **Purpose**: Handle individual bot client sessions
- **Key Functions**:
  - `handle_info/2` - Process TCP messages from clients
  - `handle_command/2` - Parse and execute bot commands
  - `parse_command/1` - Command parsing logic

### bot_manager.erl
- **Type**: Utility module
- **Purpose**: Bot state management and coordination
- **Key Functions**:
  - `register_bot/3` - Register new bot with position
  - `assign_task/2` - Assign movement tasks to bots
  - `get_bot/1` - Retrieve bot information

### grid_manager.erl
- **Type**: Utility module with Mnesia interface
- **Purpose**: Grid setup and querying
- **Key Functions**:
  - `setup_grid/0` - Initialize 9x9 grid with obstacles
  - `get_empty_positions/0` - Query available positions

### path_planner.erl
- **Type**: Algorithm module
- **Purpose**: A* pathfinding implementation  
- **Key Functions**:
  - `plan_path/2` - Calculate optimal path between coordinates
  - `astar/3` - A* algorithm implementation
  - `heuristic/2` - Manhattan distance calculation

## Data Structures

### Grid Records
```erlang
-record(cell, {key, type}).  % key = {X,Y}, type = empty | obstacle
```

### Bot Records  
```erlang
-record(bot, {id, pid, pos}).  % id = string, pid = process, pos = {X,Y}
```

## Development Workflow

### 1. Setup Development Environment
```bash
# Install Erlang/OTP 27+
# Install rebar3

# Clone project
cd bot_mind

# Initial build
make build
```

### 2. Running Tests
```bash
# Full test suite
make test

# Specific test categories  
make test-grid     # Grid functionality
make test-path     # Path planning
make test-bot      # Bot connectivity
make test-demo     # Integration demo
```

### 3. Development Cycle
```bash
# Clean development cycle
make dev           # clean + compile + test

# Quick iteration
make quick         # compile + basic test

# Check compilation
make check         # Look for errors
```

### 4. Running Server
```bash
# Production-like run
make run

# Development shell
make shell
1> application:start(bot_mind).
```

## Adding New Features

### 1. New Bot Commands

**Step 1**: Add command parsing in `bot_session.erl`
```erlang
parse_command("new_command " ++ Args) ->
    % Parse arguments
    {new_command, ParsedArgs};
```

**Step 2**: Add command handling
```erlang
handle_command(Command, State) ->
    case Command of
        {new_command, Args} ->
            % Implement logic
            "OK: Command executed";
        % ... other commands
    end.
```

**Step 3**: Add tests
```erlang
% Create test in tests/ directory
% Test the new command functionality
```

### 2. New Path Planning Algorithms

**Step 1**: Implement in `path_planner.erl`
```erlang
dijkstra_path(Start, Goal, GridMap) ->
    % Implement Dijkstra's algorithm
    Path.
```

**Step 2**: Add selection logic
```erlang
plan_path({SX,SY}, {DX,DY}, Algorithm) ->
    case Algorithm of
        astar -> astar({SX,SY}, {DX,DY}, GridMap);
        dijkstra -> dijkstra_path({SX,SY}, {DX,DY}, GridMap)
    end.
```

### 3. Grid Modifications

**Step 1**: Update grid setup in `grid_manager.erl`
```erlang
setup_grid() ->
    % Modify grid size, obstacles, or properties
    % Update obstacle positions
    Obstacles = [{2,3}, {5,5}, {7,2}, {1,8}],  % Add new obstacles
```

**Step 2**: Update coordinate validation
```erlang
validate_coordinates({X, Y}) ->
    X >= 1 andalso X =< NewGridSize andalso 
    Y >= 1 andalso Y =< NewGridSize.
```

## Testing Strategy

### Unit Tests
- Test individual module functions
- Mock external dependencies
- Test error conditions

### Integration Tests
- Test complete workflows
- Test inter-module communication  
- Test TCP protocol

### Demo Tests
- End-to-end system demonstration
- Real-world usage scenarios
- Performance validation

## Debugging

### 1. Compilation Issues
```bash
make check         # Check for syntax errors
make clean build   # Clean rebuild
```

### 2. Runtime Issues
```bash
# Enable detailed logging
make shell
1> application:start(sasl).
1> application:start(bot_mind).

# Check process status
2> supervisor:which_children(bot_mind_sup).
```

### 3. Grid/Database Issues  
```bash
# Run grid debug
make test-grid

# Check Mnesia tables
1> mnesia:info().
2> mnesia:table_info(cell, all).
```

### 4. Network Issues
```bash
# Check server status
make status

# Test connectivity
telnet localhost 5555
```

## Code Style Guidelines

### 1. Erlang Conventions
- Use snake_case for functions and variables
- Use CamelCase for modules
- Proper error handling with `{ok, Result} | {error, Reason}`

### 2. OTP Patterns
- Use gen_server for stateful processes
- Implement proper supervisor trees
- Handle system messages correctly

### 3. Documentation
- Add function documentation with -doc attributes
- Update API.md for new commands
- Include examples in documentation

## Performance Considerations

### 1. Path Planning
- A* is O(b^d) where b=branching factor, d=depth
- Consider caching for frequently used paths
- Optimize for grid size and obstacle density

### 2. Concurrent Connections
- Each bot session runs in separate process
- TCP backlog configured for multiple connections
- Consider connection pooling for high load

### 3. Database Performance
- Mnesia uses ETS tables for fast access
- Grid queries are O(n) where n=grid size
- Consider indexing for large grids

## Deployment

### 1. Release Building
```bash
make release       # Create OTP release
```

### 2. Production Configuration
- Configure proper logging levels
- Set appropriate TCP timeouts
- Configure Mnesia for disc storage

### 3. Monitoring
- Monitor supervisor restart rates
- Track connection counts
- Monitor path planning performance

## Common Issues & Solutions

### Issue: Port Already in Use
```bash
make stop          # Kill existing processes
lsof -i :5555      # Check what's using port
```

### Issue: Mnesia Schema Errors
```bash
# Delete schema and restart
rm -rf Mnesia.*
make clean build
```

### Issue: TCP Connection Refused  
```bash
# Check if server is running
make status
make run           # Start server
```

### Issue: Path Planning Returns Empty
```bash
# Check grid setup
make test-grid
# Verify start/end positions are not obstacles
```