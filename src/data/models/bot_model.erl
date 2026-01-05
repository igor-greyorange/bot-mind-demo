%% Bot data model that manages bot registration, position updates, and status tracking
%% Provides CRUD operations for bot entities in the database using centralized db_interface
-module(bot_model).
-include("schema.hrl").
-export([
    create_table/0,
    register_bot/3,
    get_bot/1,
    update_bot_position/2,
    update_bot_status/3,
    get_all_bots/0,
    get_bots_at_position/1,
    delete_bot/1,
    is_position_occupied/1
]).

%% Create bot table using common interface
create_table() ->
    db_interface:create_table(bot, record_info(fields, bot), []).

%% Register a new bot
register_bot(BotId, SessionPid, Position) ->
    BotRecord = #bot{
        id = BotId,
        session_pid = SessionPid,
        position = Position,
        status = idle,
        task = none,
        path = [],
        path_id = undefined
    },
    case db_interface:write_record(BotRecord) of
        {ok, ok} -> {ok, BotRecord};
        {error, Reason} -> {error, Reason}
    end.

%% Get bot by ID
get_bot(BotId) ->
    case db_interface:read_record(bot, BotId) of
        {ok, [BotRecord]} -> {ok, BotRecord};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% Update bot position
update_bot_position(BotId, NewPosition) ->
    case get_bot(BotId) of
        {ok, BotRecord} ->
            UpdatedBot = BotRecord#bot{position = NewPosition},
            case db_interface:write_record(UpdatedBot) of
                {ok, ok} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% Update bot status and task
update_bot_status(BotId, Status, Task) ->
    case get_bot(BotId) of
        {ok, BotRecord} ->
            UpdatedBot = BotRecord#bot{status = Status, task = Task},
            case db_interface:write_record(UpdatedBot) of
                {ok, ok} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% Get all bots
get_all_bots() ->
    db_interface:get_all_records(bot).

%% Get bots at specific position
get_bots_at_position({X, Y}) ->
    db_interface:match_objects(#bot{position = {X, Y}, _ = '_'}).

%% Check if position is occupied by any bot
is_position_occupied({X, Y}) ->
    case get_bots_at_position({X, Y}) of
        {ok, []} -> false;
        {ok, [_|_]} -> true;
        {error, _} -> false
    end.

%% Delete bot
delete_bot(BotId) ->
    case db_interface:delete_record(bot, BotId) of
        {ok, ok} -> ok;
        {error, Reason} -> {error, Reason}
    end.