#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Starting bot client...~n"),
    
    %% Connect to server
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]) of
        {ok, Socket} ->
            io:format("Connected to server!~n"),
            
            %% Test ping
            io:format("Sending ping...~n"),
            gen_tcp:send(Socket, "ping\n"),
            {ok, PingResp} = gen_tcp:recv(Socket, 0),
            io:format("Server response: ~s", [PingResp]),
            
            %% Ask for empty positions
            io:format("Requesting empty positions...~n"),
            gen_tcp:send(Socket, "get_empty_positions\n"),
            {ok, PositionsResp} = gen_tcp:recv(Socket, 0),
            io:format("Empty positions: ~s", [PositionsResp]),
            
            %% Close connection
            gen_tcp:close(Socket),
            io:format("Connection closed.~n");
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason]),
            io:format("Make sure the server is running with: rebar3 shell -> application:start(bot_mind).~n")
    end.