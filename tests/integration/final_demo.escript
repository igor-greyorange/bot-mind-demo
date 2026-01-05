#!/usr/bin/env escript
%%! -pa _build/default/lib/bot_mind/ebin

main(_Args) ->
    io:format("Final Path Planning Demo...~n"),
    
    %% Start fresh
    application:start(kernel),
    application:start(stdlib),
    application:start(mnesia),
    application:start(bot_mind),
    
    timer:sleep(1000),
    
    %% Get and show empty positions
    EmptyPos = grid_manager:get_empty_positions(),
    io:format("Available positions: ~p~n", [lists:sort(EmptyPos)]),
    
    %% Use actual empty positions for testing
    if length(EmptyPos) >= 2 ->
        [Pos1, Pos2|_] = EmptyPos,
        io:format("Testing path from ~p to ~p~n", [Pos1, Pos2]),
        Path = path_planner:plan_path(Pos1, Pos2),
        io:format("Calculated path: ~p~n", [Path]);
    true ->
        io:format("Not enough positions for path planning~n")
    end,
    
    %% Test bot management with valid positions  
    if length(EmptyPos) >= 3 ->
        [BotPos1, BotPos2, BotPos3|_] = EmptyPos,
        
        bot_manager:register_bot("bot1", self(), BotPos1),
        bot_manager:register_bot("bot2", self(), BotPos2),
        
        io:format("Registered bots at: bot1=~p, bot2=~p~n", [BotPos1, BotPos2]),
        
        %% Try to move bot1 to bot3's position
        TaskResult = bot_manager:assign_task("bot1", {move_to, BotPos3}),
        case TaskResult of
            {ok, AssignedPath} -> 
                io:format("Successfully assigned path to bot1: ~p~n", [AssignedPath]);
            Error ->
                io:format("Failed to assign path: ~p~n", [Error])
        end;
    true ->
        io:format("Not enough positions for bot management test~n")
    end,
    
    io:format("~n=== Path Planning System Ready! ===~n"),
    io:format("Connect to port 5555 and use these commands:~n"),
    io:format("- get_empty_positions~n"),
    io:format("- plan_path X,Y X,Y~n"),
    io:format("- register_bot botname X,Y~n"),
    io:format("- move_to botname X,Y~n"),
    
    application:stop(bot_mind).