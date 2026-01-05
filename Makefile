# Bot Mind - Path Planning Server
# Makefile for build, test, and run operations

# Variables
APP_NAME = bot_mind
REBAR3 = rebar3
ESCRIPT = escript

# Directories
SRC_DIR = src
TEST_DIR = tests
SCRIPT_DIR = scripts
BUILD_DIR = _build

# Colors for output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[1;33m
BLUE = \033[0;34m
NC = \033[0m # No Color

.PHONY: all build clean compile test test-unit test-integration test-demo run server help deps check format

# Default target
all: build

# Help target
help:
	@echo "$(BLUE)Bot Mind - Path Planning Server$(NC)"
	@echo "$(BLUE)==============================$(NC)"
	@echo ""
	@echo "$(GREEN)Build Commands:$(NC)"
	@echo "  make build        - Compile the application"
	@echo "  make compile      - Same as build"
	@echo "  make clean        - Clean build artifacts"
	@echo "  make deps         - Get dependencies"
	@echo ""
	@echo "$(GREEN)Run Commands:$(NC)"
	@echo "  make run          - Start the server"
	@echo "  make server       - Start the server (alias for run)"
	@echo ""
	@echo "$(GREEN)Test Commands:$(NC)"
	@echo "  make test         - Run all tests"
	@echo "  make test-unit    - Run unit tests"
	@echo "  make test-demo    - Run demo/integration tests"
	@echo "  make test-grid    - Test grid functionality"
	@echo "  make test-path    - Test path planning"
	@echo "  make test-bot     - Test bot client connection"
	@echo ""
	@echo "$(GREEN)Development Commands:$(NC)"
	@echo "  make check        - Check for compilation errors"
	@echo "  make format       - Format code (if available)"
	@echo "  make shell        - Start Erlang shell with app loaded"
	@echo ""
	@echo "$(GREEN)Cleanup Commands:$(NC)"
	@echo "  make clean-all    - Clean everything including deps"
	@echo "  make reset        - Reset to clean state"

# Build targets
build: compile

compile:
	@echo "$(YELLOW)Compiling $(APP_NAME)...$(NC)"
	@$(REBAR3) compile
	@echo "$(GREEN)✓ Compilation successful$(NC)"

deps:
	@echo "$(YELLOW)Getting dependencies...$(NC)"
	@$(REBAR3) get-deps
	@echo "$(GREEN)✓ Dependencies updated$(NC)"

# Clean targets
clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@$(REBAR3) clean
	@echo "$(GREEN)✓ Clean completed$(NC)"

clean-all:
	@echo "$(YELLOW)Cleaning everything...$(NC)"
	@rm -rf $(BUILD_DIR)
	@rm -rf _checkouts
	@echo "$(GREEN)✓ Full clean completed$(NC)"

reset: clean-all deps compile

# Check for errors
check:
	@echo "$(YELLOW)Checking for compilation errors...$(NC)"
	@$(REBAR3) compile 2>&1 | grep -i error || echo "$(GREEN)✓ No compilation errors found$(NC)"

# Run targets
run: build
	@echo "$(YELLOW)Starting $(APP_NAME) server...$(NC)"
	@echo "$(BLUE)Server will be available on port 5555$(NC)"
	@echo "$(BLUE)Press Ctrl+C to stop$(NC)"
	@$(ESCRIPT) $(SCRIPT_DIR)/start_server.escript

server: run

shell: build
	@echo "$(YELLOW)Starting Erlang shell...$(NC)"
	@echo "$(BLUE)Use: application:start($(APP_NAME)). to start the app$(NC)"
	@$(REBAR3) shell

# Test targets  
test: test-unit test-integration
	@echo "$(GREEN)✓ All tests completed$(NC)"

test-workflow:
	@echo "$(YELLOW)Running workflow tests...$(NC)"
	@echo "$(BLUE)Make sure server is running first!$(NC)"
	@$(ESCRIPT) tests/workflow/simple_test.escript



test-demo: build
	@echo "$(YELLOW)Running demo tests...$(NC)"
	@echo "$(BLUE)Testing complete system integration...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/final_demo.escript
	@echo "$(GREEN)✓ Demo tests passed$(NC)"

# Organized test categories
test-unit: build
	@echo "$(YELLOW)Running unit tests...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/unit/test_app.escript

test-integration: build
	@echo "$(YELLOW)Running integration tests...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/test_mnesia_system.escript

test-path: build
	@echo "$(YELLOW)Testing path planning system...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/test_path_planning.escript

test-grid: build
	@echo "$(YELLOW)Testing grid setup and functionality...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/debug/detailed_debug.escript

test-bot: build
	@echo "$(YELLOW)Testing bot client connection...$(NC)"
	@echo "$(BLUE)Note: Make sure server is running in another terminal$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/test_bot_client.escript

test-mnesia: build
	@echo "$(YELLOW)Testing Mnesia bot storage system...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/test_mnesia_system.escript

test-reservations: build
	@echo "$(YELLOW)Testing path reservation & collision avoidance...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/test_reservation_system.escript

# Development helpers
format:
	@echo "$(YELLOW)Code formatting not available for Erlang$(NC)"
	@echo "$(BLUE)Consider using erlfmt if available$(NC)"

# Server utilities
status:
	@echo "$(YELLOW)Checking server status...$(NC)"
	@$(ESCRIPT) $(SCRIPT_DIR)/server_status.escript

stop:
	@echo "$(YELLOW)Stopping server...$(NC)"
	@$(ESCRIPT) $(SCRIPT_DIR)/stop_server.escript

# Development workflow
dev: clean compile test
	@echo "$(GREEN)✓ Development cycle complete$(NC)"

# Quick test - just compile and run basic demo
quick: compile
	@echo "$(YELLOW)Quick test...$(NC)"
	@$(ESCRIPT) $(TEST_DIR)/integration/final_demo.escript

# Create release
release: clean compile test
	@echo "$(YELLOW)Creating release...$(NC)"
	@$(REBAR3) release
	@echo "$(GREEN)✓ Release created$(NC)"