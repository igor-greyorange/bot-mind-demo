%% Database abstraction layer that centralizes all Mnesia database operations
%% Provides consistent API for create, read, write, delete operations across all tables
-module(db_interface).
-include("models/schema.hrl").
-export([
    % Generic database operations
    create_table/3,
    read_record/2,
    write_record/1,
    delete_record/2,
    delete_objects/1,
    match_objects/1,
    transaction/1,
    clear_table/1,
    
    % Table management
    table_exists/1,
    wait_for_tables/2,
    
    % Index operations
    index_read/3,
    
    % Utility functions
    get_all_records/1,
    count_records/1
]).

%% Create table with given name, attributes and options
create_table(TableName, RecordFields, Options) ->
    case catch mnesia:table_info(TableName, attributes) of
        {'EXIT', _} ->
            io:format("Creating table '~p'~n", [TableName]),
            DefaultOptions = [
                {attributes, RecordFields},
                {ram_copies, [node()]},
                {type, set}
            ],
            FinalOptions = merge_options(DefaultOptions, Options),
            Result = mnesia:create_table(TableName, FinalOptions),
            io:format("Table '~p' created: ~p~n", [TableName, Result]),
            Result;
        _ ->
            io:format("Table '~p' already exists~n", [TableName]),
            {atomic, ok}
    end.

%% Read record by key
read_record(Table, Key) ->
    F = fun() -> mnesia:read(Table, Key) end,
    transaction(F).

%% Write record
write_record(Record) ->
    F = fun() -> mnesia:write(Record) end,
    transaction(F).

%% Delete record by table and key
delete_record(Table, Key) ->
    F = fun() -> mnesia:delete({Table, Key}) end,
    transaction(F).

%% Delete multiple objects (for bulk operations)
delete_objects(Objects) ->
    F = fun() ->
        [mnesia:delete_object(Obj) || Obj <- Objects],
        length(Objects)
    end,
    transaction(F).

%% Match objects with pattern
match_objects(Pattern) ->
    F = fun() -> mnesia:match_object(Pattern) end,
    transaction(F).

%% Execute transaction with proper error handling
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

%% Clear entire table
clear_table(TableName) ->
    F = fun() ->
        case catch mnesia:clear_table(TableName) of
            {atomic, ok} -> ok;
            _ -> 
                % Manual cleanup if automatic clear fails
                mnesia:foldl(fun(Record, _Acc) ->
                    mnesia:delete_object(Record)
                end, ok, TableName)
        end
    end,
    transaction(F).

%% Check if table exists
table_exists(TableName) ->
    case catch mnesia:table_info(TableName, attributes) of
        {'EXIT', _} -> false;
        _ -> true
    end.

%% Wait for tables to be ready
wait_for_tables(Tables, Timeout) ->
    mnesia:wait_for_tables(Tables, Timeout).

%% Index read
index_read(Table, Value, IndexField) ->
    F = fun() -> mnesia:index_read(Table, Value, IndexField) end,
    transaction(F).

%% Get all records from table
get_all_records(TableName) ->
    Pattern = case TableName of
        bot -> #bot{_ = '_'};
        cell -> #cell{_ = '_'};
        reservation -> #reservation{_ = '_'}
    end,
    match_objects(Pattern).

%% Count records in table
count_records(TableName) ->
    F = fun() -> mnesia:table_info(TableName, size) end,
    transaction(F).

%% Helper function to merge options lists
merge_options(DefaultOptions, UserOptions) ->
    % Merge user options with defaults, user options take precedence
    lists:foldl(fun({Key, Value}, Acc) ->
        lists:keystore(Key, 1, Acc, {Key, Value})
    end, DefaultOptions, UserOptions).