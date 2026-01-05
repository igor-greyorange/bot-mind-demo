%% Path reservation system that prevents bot collisions by managing exclusive position locks
%% Implements atomic path reservations and releases to coordinate multiple bot movements safely
-module(reservation_model).
-include("schema.hrl").
-export([
    setup_reservation_table/0,
    reserve_path/2,
    release_path/1,
    release_bot_reservations/1,
    is_position_reserved/1,
    get_reserving_bot/1,
    get_bot_reservations/1,
    get_all_reservations/0,
    clear_all_reservations/0
]).

%% Setup reservation table in Mnesia
setup_reservation_table() ->
    Options = [
        {index, [#reservation.bot_id, #reservation.path_id]}
    ],
    db_interface:create_table(reservation, record_info(fields, reservation), Options),
    db_interface:wait_for_tables([reservation], 5000),
    ok.

%% Reserve entire path for a bot
reserve_path(BotId, Path) ->
    case check_path_availability(Path, BotId) of
        {conflict, Position, ReservingBot} ->
            {error, {position_reserved, Position, ReservingBot}};
        available ->
            PathId = make_path_id(BotId),
            Timestamp = erlang:system_time(second),
            
            Reservations = [#reservation{
                position = Pos,
                bot_id = BotId,
                timestamp = Timestamp,
                path_id = PathId
            } || Pos <- Path],
            
            % Write all reservations using db_interface
            WriteResults = [db_interface:write_record(R) || R <- Reservations],
            SuccessCount = length([ok || {ok, ok} <- WriteResults]),
            
            case SuccessCount =:= length(Reservations) of
                true ->
                    io:format("Path reserved for bot ~s: ~p~n", [BotId, Path]),
                    {ok, PathId};
                false ->
                    io:format("Failed to reserve complete path for bot ~s~n", [BotId]),
                    {error, partial_reservation_failed}
            end
    end.

%% Release entire path reservation
release_path(PathId) ->
    case db_interface:index_read(reservation, PathId, #reservation.path_id) of
        {ok, Reservations} ->
            case db_interface:delete_objects(Reservations) of
                {ok, Count} ->
                    io:format("Released ~p positions for path ~s~n", [Count, PathId]),
                    {ok, Count};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Release all reservations for a specific bot
release_bot_reservations(BotId) ->
    case db_interface:index_read(reservation, BotId, #reservation.bot_id) of
        {ok, Reservations} ->
            case db_interface:delete_objects(Reservations) of
                {ok, Count} ->
                    io:format("Released ~p reserved positions for bot ~s~n", [Count, BotId]),
                    {ok, Count};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Check if a position is reserved
is_position_reserved({X, Y}) ->
    case db_interface:read_record(reservation, {X, Y}) of
        {ok, []} -> false;
        {ok, [_]} -> true;
        {error, _} -> false
    end.

%% Get which bot has reserved a position
get_reserving_bot({X, Y}) ->
    case db_interface:read_record(reservation, {X, Y}) of
        {ok, [#reservation{bot_id = BotId}]} -> {ok, BotId};
        {ok, []} -> {error, not_reserved};
        {error, Reason} -> {error, Reason}
    end.

%% Get all reservations for a specific bot
get_bot_reservations(BotId) ->
    case db_interface:index_read(reservation, BotId, #reservation.bot_id) of
        {ok, Reservations} ->
            Positions = [R#reservation.position || R <- Reservations],
            {ok, Positions};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get all current reservations
get_all_reservations() ->
    db_interface:get_all_records(reservation).

%% Clear all reservations (useful for debugging/reset)
clear_all_reservations() ->
    case db_interface:clear_table(reservation) of
        {ok, ok} ->
            io:format("All reservations cleared~n"),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper functions
make_path_id(BotId) ->
    Timestamp = erlang:system_time(microsecond),
    lists:flatten(io_lib:format("~s_~p", [BotId, Timestamp])).

check_path_availability(Path, BotId) ->
    check_path_positions(Path, BotId).

check_path_positions([], _BotId) ->
    available;
check_path_positions([Pos | Rest], BotId) ->
    case db_interface:read_record(reservation, Pos) of
        {ok, []} -> 
            check_path_positions(Rest, BotId);
        {ok, [#reservation{bot_id = BotId}]} -> 
            % Same bot - allow re-reservation
            check_path_positions(Rest, BotId);
        {ok, [#reservation{bot_id = OtherBot}]} -> 
            {conflict, Pos, OtherBot};
        {error, _} -> 
            check_path_positions(Rest, BotId)
    end.