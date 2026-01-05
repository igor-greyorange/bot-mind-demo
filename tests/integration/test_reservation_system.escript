#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("🚀 Testing Path Reservation & Collision Avoidance System...~n~n"),
    
    %% Start server
    application:ensure_all_started(bot_mind),
    timer:sleep(1000),
    
    %% Connect and test
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format("✅ Connected to server!~n~n"),
            
            %% Test basic reservation system
            test_basic_reservations(Socket),
            
            %% Test collision avoidance
            test_collision_avoidance(Socket),
            
            %% Test reservation management
            test_reservation_management(Socket),
            
            gen_tcp:close(Socket),
            io:format("~n🎉 All path reservation tests completed successfully!~n");
        {error, Reason} ->
            io:format("❌ Failed to connect: ~p~n", [Reason])
    end,
    
    %% Stop server
    application:stop(bot_mind).

test_basic_reservations(Socket) ->
    io:format("🔒 === Testing Basic Path Reservations ===~n"),
    
    %% Register bots
    send_and_receive(Socket, "register_bot bot1 1,1"),
    send_and_receive(Socket, "register_bot bot2 9,9"),
    
    %% Move bot1 - should succeed and reserve path
    send_and_receive(Socket, "move_to bot1 5,6"),
    
    %% Check reservations
    send_and_receive(Socket, "get_reservations"),
    send_and_receive(Socket, "get_bot_reservations bot1").

test_collision_avoidance(Socket) ->
    io:format("~n⚡ === Testing Collision Avoidance ===~n"),
    
    %% Try to move bot2 through bot1's reserved path - should fail
    send_and_receive(Socket, "move_to bot2 3,3"),
    
    %% Move bot2 to a different path that doesn't conflict
    send_and_receive(Socket, "move_to bot2 7,7"),
    
    %% Check all reservations now
    send_and_receive(Socket, "get_reservations").

test_reservation_management(Socket) ->
    io:format("~n🛠️  === Testing Reservation Management ===~n"),
    
    %% Check bot-specific reservations
    send_and_receive(Socket, "get_bot_reservations bot1"),
    send_and_receive(Socket, "get_bot_reservations bot2"),
    
    %% Complete bot1 movement - should release reservations
    send_and_receive(Socket, "complete_movement bot1"),
    
    %% Check reservations after completion
    send_and_receive(Socket, "get_reservations"),
    send_and_receive(Socket, "get_bot_reservations bot1"),
    
    %% Now bot2 should be able to move through previously blocked area
    send_and_receive(Socket, "move_to bot2 1,1"),
    
    %% Final status
    send_and_receive(Socket, "get_all_bots"),
    send_and_receive(Socket, "get_reservations").

send_and_receive(Socket, Command) ->
    io:format("   → ~s~n", [Command]),
    gen_tcp:send(Socket, [Command, "\n"]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Response} ->
            CleanResponse = string:trim(binary_to_list(Response)),
            % Color code responses
            case string:str(CleanResponse, "ERROR") of
                0 -> io:format("   ✅ ~s~n", [CleanResponse]);
                _ -> io:format("   ❌ ~s~n", [CleanResponse])
            end;
        {error, Reason} ->
            io:format("   ✗ Error: ~p~n", [Reason])
    end,
    timer:sleep(300).