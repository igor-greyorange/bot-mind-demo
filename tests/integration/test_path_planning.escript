#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Starting Path Planning Test Client...~n"),
    
    %% Connect to server
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format("Connected to server!~n"),
            
            test_basic_commands(Socket),
            test_bot_management(Socket),
            test_path_planning(Socket),
            
            %% Close connection
            gen_tcp:close(Socket),
            io:format("Test completed successfully!~n");
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason]),
            io:format("Make sure the server is running with: rebar3 shell -> application:start(bot_mind).~n")
    end.

test_basic_commands(Socket) ->
    io:format("~n=== Testing Basic Commands ===~n"),
    
    %% Test ping
    send_and_receive(Socket, "ping"),
    
    %% Get empty positions
    send_and_receive(Socket, "get_empty_positions").

test_bot_management(Socket) ->
    io:format("~n=== Testing Bot Management ===~n"),
    
    %% Register bots
    send_and_receive(Socket, "register_bot bot1 1,1"),
    send_and_receive(Socket, "register_bot bot2 1,9"),
    send_and_receive(Socket, "register_bot bot3 9,1"),
    
    %% Get all bots
    send_and_receive(Socket, "get_all_bots"),
    
    %% Get specific bot
    send_and_receive(Socket, "get_bot bot1"),
    
    %% Move a bot
    send_and_receive(Socket, "move_bot bot1 2,2").

test_path_planning(Socket) ->
    io:format("~n=== Testing Path Planning ===~n"),
    
    %% Plan a simple path
    send_and_receive(Socket, "plan_path 1,1 3,3"),
    
    %% Plan path around obstacles
    send_and_receive(Socket, "plan_path 1,1 9,9"),
    
    %% Test invalid paths
    send_and_receive(Socket, "plan_path 2,3 5,5"), % obstacle to obstacle
    
    %% Assign path to bot
    send_and_receive(Socket, "assign_path bot1 2,2 8,8"),
    
    %% Move bot to destination
    send_and_receive(Socket, "move_to bot2 5,5"),
    
    %% Check updated bot positions
    send_and_receive(Socket, "get_all_bots").

send_and_receive(Socket, Command) ->
    io:format("Sending: ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    {ok, Response} = gen_tcp:recv(Socket, 0),
    io:format("Response: ~s", [Response]),
    timer:sleep(100). %% Small delay for readability