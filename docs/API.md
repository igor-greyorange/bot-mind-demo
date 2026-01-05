# Bot Mind API Documentation

## Overview

Bot Mind provides a TCP-based API for bot coordination and path planning. All communication happens over TCP on port 5555 using line-based protocol (commands end with `\n`).

## Connection

```bash
telnet localhost 5555
# or use any TCP client
```

## Command Format

All commands are text-based and case-sensitive. Parameters are separated by spaces.

## Basic Commands

### ping
Test server connectivity.

**Request:**
```
ping
```

**Response:**
```
pong
```

### get_empty_positions
Get all available positions on the grid.

**Request:**
```
get_empty_positions
```

**Response:**
```
[{1,1},{1,2},{1,3},...,{9,9}]  # List of {X,Y} coordinates
```

## Path Planning Commands

### plan_path
Calculate optimal path between two coordinates.

**Request:**
```
plan_path <startX>,<startY> <endX>,<endY>
```

**Example:**
```
plan_path 1,1 9,9
```

**Response:**
```
[{1,1},{2,1},{3,1},...,{9,9}]  # Optimal path coordinates
```

**Error Response:**
```
ERROR: No path found
ERROR: Invalid coordinates
```

## Bot Management Commands

### register_bot
Register a new bot with the system.

**Request:**
```
register_bot <botId> <x>,<y>
```

**Example:**
```
register_bot robot1 3,4
```

**Response:**
```
OK: Bot registered
ERROR: Invalid position
```

### move_bot
Update a bot's position manually.

**Request:**
```
move_bot <botId> <x>,<y>
```

**Example:**
```
move_bot robot1 5,7
```

**Response:**
```
OK: Bot position updated
ERROR: Bot not found
```

### move_to
Move bot to destination (automatic path planning).

**Request:**
```
move_to <botId> <x>,<y>
```

**Example:**
```
move_to robot1 8,8
```

**Response:**
```
OK: Bot moving to (8,8) via [{3,4},{4,4},{5,4},...,{8,8}]
ERROR: No path found
ERROR: Bot not found
```

### assign_path
Assign specific path from source to destination.

**Request:**
```
assign_path <botId> <startX>,<startY> <endX>,<endY>
```

**Example:**
```
assign_path robot1 1,1 9,9
```

**Response:**
```
OK: Path assigned - [{1,1},{2,1},{3,1},...,{9,9}]
ERROR: No path found
ERROR: Bot not found
```

### get_bot
Get information about a specific bot.

**Request:**
```
get_bot <botId>
```

**Example:**
```
get_bot robot1
```

**Response:**
```
{bot,"robot1",<0.123.0>,{5,7}}  # Bot record: {bot, ID, Pid, Position}
ERROR: Bot not found
```

### get_all_bots
Get information about all registered bots.

**Request:**
```
get_all_bots
```

**Response:**
```
[{"robot1",{bot,"robot1",<0.123.0>,{5,7}}},
 {"robot2",{bot,"robot2",<0.124.0>,{2,3}}}]
[]  # Empty list if no bots registered
```

## Error Handling

### Common Error Responses

- `ERROR: Invalid command` - Unknown command
- `ERROR: Invalid format` - Malformed command syntax
- `ERROR: Bot not found` - Bot ID doesn't exist
- `ERROR: Invalid coordinates` - Coordinates out of bounds (not 1-9)
- `ERROR: No path found` - Path planning failed (blocked positions)

### Command Format Errors

Each command has specific format requirements:

```
ERROR: Invalid plan_path format. Use: plan_path 1,2 5,6
ERROR: Invalid register_bot format. Use: register_bot bot1 3,4
ERROR: Invalid move_bot format. Use: move_bot bot1 5,7
ERROR: Invalid move_to format. Use: move_to bot1 5,7
ERROR: Invalid assign_path format. Use: assign_path bot1 1,2 8,9
```

## Grid System

### Coordinate System
- Grid size: 9×9 (coordinates 1-9)
- Origin: (1,1) at top-left
- X-axis: horizontal (1-9, left to right)
- Y-axis: vertical (1-9, top to bottom)

### Obstacles
Fixed obstacles at coordinates:
- (2,3)
- (5,5) 
- (7,2)

### Path Planning
- Algorithm: A* with Manhattan distance heuristic
- Movement: 4-directional (up, down, left, right)
- Paths avoid obstacles and stay within grid bounds

## Example Session

```
$ telnet localhost 5555
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.

> ping
pong

> get_empty_positions
[{1,1},{1,2},{1,3},...{9,9}]

> register_bot mybot 1,1
OK: Bot registered

> move_to mybot 9,9
OK: Bot moving to (9,9) via [{1,1},{2,1},{3,1},{4,1},{5,1},{6,1},{7,1},{8,1},{9,1},{9,2},{9,3},{9,4},{9,5},{9,6},{9,7},{9,8},{9,9}]

> get_bot mybot
{bot,"mybot",<0.145.0>,{9,9}}

> plan_path 1,1 5,5
ERROR: End position (5,5) is blocked

> plan_path 1,1 5,4
[{1,1},{2,1},{3,1},{4,1},{5,1},{5,2},{5,3},{5,4}]
```

## Integration Examples

### Python Client
```python
import socket

def send_command(sock, command):
    sock.send((command + '\n').encode())
    return sock.recv(1024).decode().strip()

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('localhost', 5555))

# Register bot
response = send_command(sock, "register_bot python_bot 1,1")
print(response)  # OK: Bot registered

# Move bot
response = send_command(sock, "move_to python_bot 8,8")
print(response)  # OK: Bot moving to (8,8) via [...]

sock.close()
```

### JavaScript/Node.js Client
```javascript
const net = require('net');

const client = new net.Socket();

client.connect(5555, 'localhost', () => {
    console.log('Connected to Bot Mind server');
    
    // Register bot
    client.write('register_bot js_bot 2,2\n');
});

client.on('data', (data) => {
    console.log('Server response:', data.toString().trim());
});

client.on('close', () => {
    console.log('Connection closed');
});
```

## Performance Notes

- Server handles multiple concurrent connections
- Path planning is computed in real-time
- Grid state is persistent via Mnesia database
- Bot positions are stored in process memory for fast access

## Security Considerations

- No authentication currently implemented
- Server accepts connections from localhost
- Commands are plain text (consider encryption for production)
- Bot IDs should be unique per client session