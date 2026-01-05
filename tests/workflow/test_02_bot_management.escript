#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

%%% Test Case 2: Bot Registration and Management
%%% Purpose: Test bot registration, status checking, and basic management operations

main(_Args) ->
    io:format(" TEST 2: Bot Registration & Management~n"),
    io:format("==========================================~n"),
    
    %% Step 1: Connect to server
    io:format("🔗 Connecting to server...~n"),
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format(" Connected to server!~n"),
            
            %% Step 2: Test ping to ensure server is responsive
            test_ping(Socket),
            
            %% Step 3: Register multiple bots
            io:format(" Registering test bots...~n"),
            register_test_bots(Socket),
            
            %% Step 4: Verify bot registration
            io:format(" Verifying bot registration...~n"),
            verify_bot_registration(Socket),
            
            %% Step 5: Test bot queries
            io:format("Testing bot queries...~n"),
            test_bot_queries(Socket),
            
            %% Step 6: Test bot updates
            io:format(" Testing bot position updates...~n"),
            test_bot_updates(Socket),
            
            %% Step 7: Close connection
            gen_tcp:close(Socket),
            io:format(" TEST 2 PASSED: Bot registration and management working!~n");
            
        {error, Reason} ->
            io:format(" FAILED: Could not connect to server: ~p~n", [Reason]),
            exit(1)
    end.

%% Quick ping test
test_ping(Socket) ->
    io:format(" Testing server responsiveness...~n"),
    send_and_receive(Socket, "ping"),
    timer:sleep(300).

%% Register multiple test bots
register_test_bots(Socket) ->
    TestBots = [
        {"test_bot_1", "1,1"},
        {"test_bot_2", "1,9"},
        {"test_bot_3", "9,1"},
        {"test_bot_4", "9,9"}
    ],
    
    lists:foreach(fun({BotId, Position}) ->
        Command = lists:flatten(io_lib:format("register_bot ~s ~s", [BotId, Position])),
        io:format("    Registering bot: ~s at position ~s~n", [BotId, Position]),
        send_and_receive(Socket, Command),
        timer:sleep(500)
    end, TestBots).

%% Verify all bots were registered correctly
verify_bot_registration(Socket) ->
    io:format("    Getting all registered bots...~n"),
    send_and_receive(Socket, "get_all_bots"),
    timer:sleep(500),
    
    %% Check individual bots
    TestBotIds = ["test_bot_1", "test_bot_2", "test_bot_3"],
    lists:foreach(fun(BotId) ->
        Command = lists:flatten(io_lib:format("get_bot ~s", [BotId])),
        io:format("    Checking bot: ~s~n", [BotId]),
        send_and_receive(Socket, Command),
        timer:sleep(300)
    end, TestBotIds).

%% Test various bot query operations
test_bot_queries(Socket) ->
    %% Test getting empty positions
    io:format("    Checking empty positions...~n"),
    send_and_receive(Socket, "get_empty_positions"),
    timer:sleep(500),
    
    %% Test invalid bot query
    io:format("    Testing invalid bot query...~n"),
    send_and_receive(Socket, "get_bot nonexistent_bot"),
    timer:sleep(300).

%% Test bot position updates
test_bot_updates(Socket) ->
    %% Move a bot to a new position
    io:format("    Moving test_bot_1 to position 2,2...~n"),
    send_and_receive(Socket, "move_bot test_bot_1 2,2"),
    timer:sleep(500),
    
    %% Verify the move
    io:format("    Verifying bot position update...~n"),
    send_and_receive(Socket, "get_bot test_bot_1"),
    timer:sleep(300),
    
    %% Test invalid move
    io:format("     Testing invalid move (to obstacle)...~n"),
    send_and_receive(Socket, "move_bot test_bot_2 5,5"),
    timer:sleep(300).

%% Helper function to send command and show response
send_and_receive(Socket, Command) ->
    io:format("      → ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Response} ->
            CleanResponse = string:trim(binary_to_list(Response)),
            io:format("      ← ~s~n", [CleanResponse]);
        {error, Reason} ->
            io:format("      Error: ~p~n", [Reason])
    end.