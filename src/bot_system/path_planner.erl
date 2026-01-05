%% A-star pathfinding algorithm implementation for bot navigation on a 9x9 grid
%% Calculates optimal paths from source to destination while avoiding obstacles
-module(path_planner).
-include("../data/models/schema.hrl").
-export([plan_path/2]).

plan_path({SX,SY}, {DX,DY}) ->
    %% Validate input coordinates
    case validate_coordinates({SX,SY}) andalso validate_coordinates({DX,DY}) of
        false ->
            io:format("Invalid coordinates: Start ~p, End ~p~n", [{SX,SY}, {DX,DY}]),
            [];
        true ->
            case grid_model:get_all_cells() of
                {ok, Cells} ->
                    GridMap = lists:foldl(fun(#cell{key={X,Y}, type=Type}, Acc) ->
                                               maps:put({X,Y}, Type, Acc)
                                           end, #{}, Cells),
                    
                    %% Check if start and end positions are valid (not obstacles)
                    case {maps:get({SX,SY}, GridMap, obstacle), maps:get({DX,DY}, GridMap, obstacle)} of
                        {empty, empty} ->
                            astar({SX,SY}, {DX,DY}, GridMap);
                        {obstacle, _} ->
                            io:format("Start position ~p is blocked~n", [{SX,SY}]),
                            [];
                        {_, obstacle} ->
                            io:format("End position ~p is blocked~n", [{DX,DY}]),
                            [];
                        _ ->
                            io:format("Unknown error with positions~n"),
                            []
                    end;
                {error, Reason} ->
                    io:format("Failed to read grid: ~p~n", [Reason]),
                    []
            end
    end.

%% Validate that coordinates are within grid bounds (1-9)
validate_coordinates({X, Y}) ->
    X >= 1 andalso X =< 9 andalso Y >= 1 andalso Y =< 9.

heuristic({X1,Y1}, {X2,Y2}) -> abs(X1-X2)+abs(Y1-Y2).

neighbors({X,Y}, GridMap) ->
    Potential = [{X+1,Y},{X-1,Y},{X,Y+1},{X,Y-1}],
    lists:filter(fun(Pos) -> maps:get(Pos, GridMap, obstacle) =:= empty end, Potential).

astar(Start, Goal, GridMap) ->
    Open = [{Start,0,heuristic(Start,Goal),[Start]}],
    astar_loop(Open, [], Goal, GridMap).

astar_loop([], _Closed, _Goal, _Grid) -> [];
astar_loop([{Curr,G,_F,Path}|Rest], Closed, Goal, GridMap) ->
    if Curr =:= Goal -> lists:reverse(Path);
       true ->
           Closed1 = [Curr|Closed],
           Neigh = lists:filter(fun(N)-> not lists:member(N,Closed1) end, neighbors(Curr,GridMap)),
           Open1 = Rest ++ [{N,G+1,G+1+heuristic(N,Goal),[N|Path]} || N<-Neigh],
           OpenSorted = lists:sort(fun({_,_,F1,_},{_,_,F2,_})-> F1 =< F2 end, Open1),
           astar_loop(OpenSorted, Closed1, Goal, GridMap)
    end.
