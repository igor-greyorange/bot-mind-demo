%% Grid data model that manages the 9x9 game world including obstacle placement and empty position tracking
%% Handles cell creation, grid setup, and spatial queries for navigation system
-module(grid_model).
-include("schema.hrl").
-export([
    create_table/0,
    setup_grid/0,
    get_empty_positions/0,
    get_cell/1,
    is_obstacle/1,
    get_all_cells/0
]).

%% Create cell table using common interface
create_table() ->
    db_interface:create_table(cell, record_info(fields, cell), []).

%% Setup 9x9 grid with obstacles
setup_grid() ->
    Obstacles = [{2,3}, {5,5}, {7,2}],
    
    % Clear existing data
    case db_interface:clear_table(cell) of
        {ok, _} -> 
            io:format("Cleared existing cell table data~n");
        {error, Reason} -> 
            io:format("Error clearing table: ~p~n", [Reason])
    end,
    
    % Create grid records
    Records = [
        #cell{key={X,Y}, type=case lists:member({X,Y}, Obstacles) of
                                 true -> obstacle; 
                                 false -> empty 
                             end}
        || X <- lists:seq(1,9), Y <- lists:seq(1,9)
    ],
    
    % Write all records
    Results = [db_interface:write_record(Record) || Record <- Records],
    SuccessCount = length([ok || {ok, _} <- Results]),
    io:format("Grid setup complete: ~p/81 cells created~n", [SuccessCount]).

%% Get all empty positions
get_empty_positions() ->
    case db_interface:match_objects(#cell{type = empty, _ = '_'}) of
        {ok, Cells} -> 
            Positions = [Key || #cell{key = Key} <- Cells],
            {ok, Positions};
        {error, Reason} -> {error, Reason}
    end.

%% Get cell information
get_cell({X, Y}) ->
    db_interface:read_record(cell, {X, Y}).

%% Check if position is obstacle
is_obstacle({X, Y}) ->
    case get_cell({X, Y}) of
        {ok, [#cell{type = obstacle}]} -> true;
        {ok, [#cell{type = empty}]} -> false;
        _ -> true  % Default to obstacle if unknown
    end.

%% Get all cells
get_all_cells() ->
    db_interface:get_all_records(cell).