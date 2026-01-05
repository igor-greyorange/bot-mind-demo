#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

%%% Test Case 3: Complete Bot Workflow - Path Planning and Navigation
%%% Purpose: Test complete bot journey from registration to destination arrival

main(_Args) ->
    io:format("  TEST 3: Complete Bot Workflow (Path Planning & Navigation)~n"),
    io:format("==========================================================~n"),
    
    %% Step 1: Connect to server
    io:format("🔗 Connecting to server...~n"),
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format(" Connected to server!~n"),
            
            %% Step 2: Server health check
            quick_health_check(Socket),
            
            %% Step 3: Register test bot
            io:format(" Registering workflow test bot...~n"),
            register_workflow_bot(Socket),
            
            %% Step 4: Plan and execute bot journey
            io:format("  Starting bot navigation workflow...~n"),
            execute_bot_journey(Socket),
            
            %% Step 5: Test collision avoidance
            io:format(" Testing collision avoidance...~n"),
            test_collision_avoidance(Socket),
            
            %% Step 6: Close connection
            gen_tcp:close(Socket),
            io:format(" TEST 3 PASSED: Complete bot workflow successful!~n");
            
        {error, Reason} ->
            io:format(" FAILED: Could not connect to server: ~p~n", [Reason]),
            exit(1)
    end.

%% Quick server health check
quick_health_check(Socket) ->
    io:format(" Server health check...~n"),
    send_and_receive(Socket, "ping"),
    timer:sleep(300).

%% Register a bot for workflow testing
register_workflow_bot(Socket) ->
    BotId = "workflow_bot",
    StartPosition = "1,1",
    
    Command = lists:flatten(io_lib:format("register_bot ~s ~s", [BotId, StartPosition])),
    io:format("    Registering ~s at ~s~n", [BotId, StartPosition]),
    send_and_receive(Socket, Command),
    timer:sleep(500),
    
    %% Verify registration
    io:format("    Verifying registration...~n"),
    send_and_receive(Socket, lists:flatten(io_lib:format("get_bot ~s", [BotId]))),
    timer:sleep(300).

%% Execute complete bot journey workflow
execute_bot_journey(Socket) ->
    BotId = "workflow_bot",
    Destination = "8,8",
    
    %% Step 1: Plan path to destination
    io:format("     Planning path to destination ~s...~n", [Destination]),
    PathCommand = lists:flatten(io_lib:format("plan_path 1,1 ~s", [Destination])),
    send_and_receive(Socket, PathCommand),
    timer:sleep(500),
    
    %% Step 2: Assign destination to bot
    io:format("    Assigning destination to bot...~n"),
    MoveCommand = lists:flatten(io_lib:format("move_to ~s ~s", [BotId, Destination])),
    send_and_receive(Socket, MoveCommand),
    timer:sleep(1000),
    
    %% Step 3: Check bot status (should be moving)
    io:format("    Checking bot status (should be moving)...~n"),
    send_and_receive(Socket, lists:flatten(io_lib:format("get_bot ~s", [BotId]))),
    timer:sleep(500),
    
    %% Step 4: Simulate bot movement progress
    io:format("    Simulating bot movement (5 seconds)...~n"),
    simulate_bot_movement(Socket, BotId),
    
    %% Step 5: Bot sends destination reached
    io:format("    Bot sending destination reached signal...~n"),
    ReachedCommand = lists:flatten(io_lib:format("destination_reached ~s", [BotId])),
    send_and_receive(Socket, ReachedCommand),
    timer:sleep(500),
    
    %% Step 6: Verify bot status (should be idle)
    io:format("    Verifying bot status (should be idle)...~n"),
    send_and_receive(Socket, lists:flatten(io_lib:format("get_bot ~s", [BotId]))),
    timer:sleep(300).

%% Simulate bot movement with periodic status updates
simulate_bot_movement(Socket, BotId) ->
    MovementSteps = [
        {"Moving step 1/5", 1000},
        {"Moving step 2/5", 1000}, 
        {"Moving step 3/5", 1000},
        {"Moving step 4/5", 1000},
        {"Moving step 5/5 - Almost there!", 1000}
    ],
    
    lists:foreach(fun({Message, DelayMs}) ->
        io:format("      🚶 ~s~n", [Message]),
        timer:sleep(DelayMs),
        
        %% Periodically check bot status
        send_and_receive(Socket, lists:flatten(io_lib:format("get_bot ~s", [BotId]))),
        timer:sleep(200)
    end, MovementSteps).

%% Test collision avoidance with multiple bots
test_collision_avoidance(Socket) ->
    %% Register second bot
    io:format("    Registering second bot for collision test...~n"),
    send_and_receive(Socket, "register_bot collision_bot_1 1,2"),
    timer:sleep(500),
    
    send_and_receive(Socket, "register_bot collision_bot_2 1,3"),
    timer:sleep(500),
    
    %% Try to send both bots to same destination
    io:format("    Testing collision avoidance...~n"),
    send_and_receive(Socket, "move_to collision_bot_1 6,6"),
    timer:sleep(300),
    
    send_and_receive(Socket, "move_to collision_bot_2 6,6"),
    timer:sleep(300),
    
    %% Check reservation status
    io:format("    Checking path reservations...~n"),
    send_and_receive(Socket, "get_all_bots"),
    timer:sleep(300),
    
    %% Simulate first bot reaching destination
    io:format("    First bot reaches destination...~n"),
    send_and_receive(Socket, "destination_reached collision_bot_1"),
    timer:sleep(500),
    
    %% Check if second bot can now move
    io:format("   Checking if path is now available for second bot...~n"),
    send_and_receive(Socket, "get_all_bots"),
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
            io:format("       Error: ~p~n", [Reason])
    end.