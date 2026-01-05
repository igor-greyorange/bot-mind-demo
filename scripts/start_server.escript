#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Starting bot_mind server...~n"),
    
    %% Start the application with all dependencies
    case application:ensure_all_started(bot_mind) of
        {ok, _Apps} -> 
            io:format("Server started successfully on port 5555!~n"),
            io:format("Press Ctrl+C to stop the server~n"),
            %% Keep the server running
            receive
                _ -> ok
            end;
        {error, Reason} -> 
            io:format("Failed to start server: ~p~n", [Reason])
    end.