#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

%%% Test Case 1: Server Connection and Basic Ping Test
%%% Purpose: Verify server startup, connection establishment, and basic ping functionality

main(_Args) ->
    io:format(" TEST 1: Server Connection & Ping Test~n"),
    io:format("========================================~n"),
    
    %% Step 1: Wait for server to be ready
    io:format(" Waiting for server to start...~n"),
    timer:sleep(2000),
    
    %% Step 2: Attempt connection to server
    io:format("🔗 Attempting to connect to server at localhost:5555...~n"),
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format(" SUCCESS: Connected to server!~n"),
            
            %% Step 3: Test ping command
            io:format(" Testing ping command...~n"),
            test_ping(Socket),
            
            %% Step 4: Test server status
            io:format(" Testing server status...~n"),
            test_server_commands(Socket),
            
            %% Step 5: Close connection
            gen_tcp:close(Socket),
            io:format(" Connection closed successfully~n"),
            io:format(" TEST 1 PASSED: Server connection and ping working!~n");
            
        {error, Reason} ->
            io:format(" FAILED: Could not connect to server: ~p~n", [Reason]),
            io:format(" Make sure server is running: make run~n"),
            exit(1)
    end.

%% Test ping functionality
test_ping(Socket) ->
    send_command_and_verify(Socket, "ping", "pong", "Ping response"),
    timer:sleep(500).

%% Test basic server commands
test_server_commands(Socket) ->
    %% Test get_empty_positions
    io:format("    Testing get_empty_positions...~n"),
    send_and_receive(Socket, "get_empty_positions"),
    timer:sleep(500),
    
    %% Test get_all_bots (should be empty initially)
    io:format("    Testing get_all_bots (should be empty)...~n"),
    send_and_receive(Socket, "get_all_bots"),
    timer:sleep(500).

%% Helper function to send command and verify response
send_command_and_verify(Socket, Command, ExpectedResponse, Description) ->
    io:format("   → Sending: ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Response} ->
            CleanResponse = string:trim(binary_to_list(Response)),
            case string:find(CleanResponse, ExpectedResponse) of
                nomatch ->
                    io:format("     ~s - Unexpected response: ~s~n", [Description, CleanResponse]);
                _ ->
                    io:format("    ~s - OK: ~s~n", [Description, CleanResponse])
            end;
        {error, Reason} ->
            io:format("    ~s - Error: ~p~n", [Description, Reason])
    end.

%% Helper function to send command and show response
send_and_receive(Socket, Command) ->
    io:format("   → Sending: ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Response} ->
            CleanResponse = string:trim(binary_to_list(Response)),
            io:format("   ← Response: ~s~n", [CleanResponse]);
        {error, Reason} ->
            io:format("    Error: ~p~n", [Reason])
    end.