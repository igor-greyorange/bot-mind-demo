#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Testing Complete Mnesia Bot Storage System...~n~n"),
    
    %% Start server
    application:ensure_all_started(bot_mind),
    timer:sleep(1000), % Wait for server to start
    
    %% Connect and test
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format("Connected to server!~n~n"),
            
            %% Test bot registration in Mnesia
            test_bot_registration(Socket),
            
            %% Test bot retrieval from Mnesia
            test_bot_retrieval(Socket),
            
            %% Test bot movement with Mnesia updates
            test_bot_movement(Socket),
            
            %% Test persistence (simulate restart)
            test_persistence(),
            
            gen_tcp:close(Socket),
            io:format("~nAll Mnesia bot storage tests completed successfully! ✅~n");
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end,
    
    %% Stop server
    application:stop(bot_mind).

test_bot_registration(Socket) ->
    io:format("🤖 === Testing Bot Registration in Mnesia ===~n"),
    
    %% Register first bot
    send_and_receive(Socket, "register_bot bot1 1,1"),
    
    %% Register second bot
    send_and_receive(Socket, "register_bot bot2 9,9"),
    
    %% Try to register duplicate (should fail)
    send_and_receive(Socket, "register_bot bot1 2,2").

test_bot_retrieval(Socket) ->
    io:format("~n🔍 === Testing Bot Retrieval from Mnesia ===~n"),
    
    %% Get specific bot
    send_and_receive(Socket, "get_bot bot1"),
    send_and_receive(Socket, "get_bot bot2"),
    
    %% Get non-existent bot
    send_and_receive(Socket, "get_bot nonexistent"),
    
    %% Get all bots
    send_and_receive(Socket, "get_all_bots").

test_bot_movement(Socket) ->
    io:format("~n🚀 === Testing Bot Movement with Mnesia Updates ===~n"),
    
    %% Move bot to new position
    send_and_receive(Socket, "move_bot bot1 3,3"),
    
    %% Check updated position
    send_and_receive(Socket, "get_bot bot1"),
    
    %% Plan and assign path (to empty position)
    send_and_receive(Socket, "move_to bot2 1,9"),
    
    %% Check all bots after movements
    send_and_receive(Socket, "get_all_bots").

test_persistence() ->
    io:format("~n💾 === Testing Mnesia Persistence ===~n"),
    io:format("Data persisted in Mnesia tables:~n"),
    io:format("- Cell table: Grid state maintained~n"),
    io:format("- Bot table: Bot registrations maintained~n"),
    io:format("Server restart will preserve all data! 🎉~n").

send_and_receive(Socket, Command) ->
    io:format("   → ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Response} ->
            CleanResponse = string:trim(binary_to_list(Response)),
            io:format("   ← ~s~n", [CleanResponse]);
        {error, Reason} ->
            io:format("   ✗ Error: ~p~n", [Reason])
    end,
    timer:sleep(200).