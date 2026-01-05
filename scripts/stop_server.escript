#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Stopping bot_mind server...~n"),
    
    case application:stop(bot_mind) of
        ok -> 
            io:format("Server stopped successfully.~n");
        {error, {not_started, bot_mind}} ->
            io:format("Server was not running.~n");
        {error, Reason} ->
            io:format("Failed to stop server: ~p~n", [Reason])
    end.