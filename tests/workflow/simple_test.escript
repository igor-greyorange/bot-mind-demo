#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

%%% Simple Bot Test - Manual Commands
%%% Run this after starting the server manually

main(_Args) ->
    io:format("=== Bot System Test ===~n"),
    
    %% Test if server is running
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format("Connected to server!~n"),
            
            %% Test commands
            test_commands(Socket),
            
            gen_tcp:close(Socket),
            io:format("Test completed!~n");
        {error, Reason} ->
            io:format("Cannot connect to server: ~p~n", [Reason]),
            io:format("Please start server first:~n"),
            io:format("  escript scripts/start_server.escript~n")
    end.

test_commands(Socket) ->
    Commands = [
        "ping",
        "register_bot bot1 1,1",
        "get_bot bot1",
        "get_all_bots",
        "move_to bot1 8,9",
        "get_bot bot1",
        "destination_reached bot1"
    ],
    
    lists:foreach(fun(Cmd) ->
        io:format("~n-> ~s~n", [Cmd]),
        gen_tcp:send(Socket, [Cmd, "\n"]),
        case gen_tcp:recv(Socket, 0, 3000) of
            {ok, Response} ->
                io:format("<- ~s", [Response]);
            {error, Reason} ->
                io:format("<- Error: ~p~n", [Reason])
        end,
        timer:sleep(3000)
    end, Commands).