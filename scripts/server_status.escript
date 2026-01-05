#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Bot Mind Server Status Check...~n~n"),
    
    %% Check if server is responding on port 5555
    case gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}], 1000) of
        {ok, Socket} ->
            io:format("✅ Server is running on port 5555~n"),
            
            %% Test basic functionality
            gen_tcp:send(Socket, "ping\n"),
            case gen_tcp:recv(Socket, 0, 1000) of
                {ok, <<"pong", _/binary>>} ->
                    io:format("✅ Server is responding correctly~n");
                {ok, Response} ->
                    io:format("⚠️  Server responding but unexpected: ~s~n", [Response]);
                {error, Reason} ->
                    io:format("❌ Server not responding: ~p~n", [Reason])
            end,
            
            gen_tcp:close(Socket);
        {error, econnrefused} ->
            io:format("❌ Server is not running (connection refused)~n");
        {error, Reason} ->
            io:format("❌ Cannot connect to server: ~p~n", [Reason])
    end,
    
    %% Check Mnesia status
    case mnesia:system_info(is_running) of
        yes ->
            io:format("✅ Mnesia database is running~n");
        no ->
            io:format("❌ Mnesia database is not running~n")
    end.