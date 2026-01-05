%% Handles individual bot communication sessions and processes incoming commands
%% Manages command parsing, bot operations, and response formatting for connected clients
-module(bot_session).
-behaviour(gen_server).
-include("../data/models/schema.hrl").
-export([start/1, send/2]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start(Socket) -> gen_server:start_link(?MODULE, Socket, []).

send(Pid, Msg) -> gen_server:cast(Pid,{send,Msg}).

init(Socket) ->
    process_flag(trap_exit, true),
    inet:setopts(Socket,[{active,once}]),
    {ok, #{socket => Socket}}.

handle_info({tcp, Socket, Data}, State) ->
    %% Parse and handle bot commands
    Command = string:trim(binary_to_list(Data)),
    io:format("Bot session received: ~s~n", [Command]),
    
    Response = handle_command(Command, State),
    gen_tcp:send(Socket, [Response, "\n"]),
    
    inet:setopts(Socket,[{active,once}]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

%% Handle server-initiated commands
handle_info({server_command, Command}, State = #{socket := Socket}) ->
    %io:format("CLIENT: Received server command: ~s~n", [string:trim(Command)]),
    
    % Set flag to indicate this is a server command (no duplicate processing)
    put(server_command_flag, true),
    
    % Process the server command and send response to client
    Response = handle_command(string:trim(Command), State),
    gen_tcp:send(Socket, [Response, "\n"]),
    
    io:format("CLIENT: Sent response to bot client: ~s~n", [string:trim(Response)]),
    
    {noreply, State}.

handle_cast({send, Msg}, State = #{socket := Socket}) ->
    gen_tcp:send(Socket, Msg),
    {noreply, State}.

handle_call(_Req,_From,State) -> {reply,ok,State}.
terminate(_Reason, #{socket := Socket}) -> catch gen_tcp:close(Socket), ok.
code_change(_V, State, _E) -> {ok,State}.

%% Command handling functions
handle_command(Command, _State) ->
    case parse_command(Command) of
        {get_empty_positions} ->
            EmptyPos = database_interface:get_empty_positions(),
            io_lib:format("~p", [EmptyPos]);
        
        {plan_path, {SX, SY}, {DX, DY}} ->
            case path_planner:plan_path({SX, SY}, {DX, DY}) of
                [] -> 
                    "ERROR: No path found";
                Path -> 
                    PathStr = format_path(Path),
                    io_lib:format("PATH: ~s", [PathStr])
            end;
        
        {register_bot, BotId, {X, Y}} ->
            case bot_manager:register_bot(BotId, self(), {X, Y}) of
                ok -> "OK: Bot registered";
                Error -> io_lib:format("ERROR: ~p", [Error])
            end;
        
        {move_bot, BotId, {X, Y}} ->
            case bot_manager:update_bot_pos(BotId, {X, Y}) of
                ok -> "OK: Bot position updated";
                Error -> io_lib:format("ERROR: ~p", [Error])
            end;
        
        {assign_path, BotId, {SX, SY}, {DX, DY}} ->
            case bot_manager:assign_task(BotId, {move, {SX, SY}, {DX, DY}}) of
                {ok, Path, PathId} -> 
                    io_lib:format("OK: Path assigned and reserved - ~s via ~s", 
                                 [format_path(Path), PathId]);
                {ok, Path} -> 
                    io_lib:format("OK: Path assigned - ~s", [format_path(Path)]);
                {error, {path_blocked, Position, ReservingBot}} ->
                    io_lib:format("ERROR: Path blocked at ~p by bot ~s", [Position, ReservingBot]);
                Error -> io_lib:format("ERROR: ~p", [Error])
            end;
        
        {move_to, BotId, {X, Y}} ->
            % Check if this is a server-initiated command (already processed) or client command
            case get(server_command_flag) of
                true ->
                    % This is from server - just display, don't process again
                    put(server_command_flag, false),  % Reset flag
                    io:format("CLIENT: Received movement instruction for bot ~s to ~p~n", [BotId, {X, Y}]),
                    io:format("CLIENT: Bot ~s starting movement to destination ~p~n", [BotId, {X, Y}]),
                    io_lib:format("OK: Bot ~s moving to ~p (Server command received)", [BotId, {X, Y}]);
                _ ->
                    % This is direct client command - process normally
                    io:format("CLIENT: Processing move command for bot ~s to ~p~n", [BotId, {X, Y}]),
                    case bot_manager:assign_task(BotId, {move_to, {X, Y}}) of
                        {ok, Path, PathId} -> 
                            io:format("CLIENT: Bot ~s got path successfully! Path: ~p~n", [BotId, Path]),
                            io_lib:format("OK: Bot moving to ~p via ~s (Reserved: ~s)", 
                                         [{X, Y}, format_path(Path), PathId]);
                        {ok, Path} -> 
                            io:format("CLIENT: Bot ~s got path successfully! Path: ~p~n", [BotId, Path]),
                            io_lib:format("OK: Bot moving to ~p via ~s", [{X, Y}, format_path(Path)]);
                        {error, {path_blocked, Position, ReservingBot}} ->
                            io:format("CLIENT: Bot ~s path blocked at ~p by bot ~s~n", [BotId, Position, ReservingBot]),
                            io_lib:format("ERROR: Path blocked at ~p by bot ~s", [Position, ReservingBot]);
                        Error -> 
                            io:format("CLIENT: Bot ~s movement failed: ~p~n", [BotId, Error]),
                            io_lib:format("ERROR: ~p", [Error])
                    end
            end;
        
        {get_all_bots} ->
            case bot_manager:get_all_bots() of
                {ok, Bots} -> io_lib:format("~p", [Bots]);
                {error, Reason} -> io_lib:format("ERROR: ~p", [Reason])
            end;
        
        {get_bot, BotId} ->
            case bot_manager:get_bot(BotId) of
                {ok, Bot} -> io_lib:format("~p", [Bot]);
                {error, bot_not_found} -> "ERROR: Bot not found";
                {error, Reason} -> io_lib:format("ERROR: ~p", [Reason])
            end;
        
        {get_reservations} ->
            case reservation_model:get_all_reservations() of
                {ok, []} -> "No active reservations";
                {ok, Reservations} ->
                    format_reservations(Reservations);
                {error, Reason} ->
                    io_lib:format("ERROR: ~p", [Reason])
            end;
            
        {get_bot_reservations, BotId} ->
            case reservation_model:get_bot_reservations(BotId) of
                {ok, []} -> 
                    io_lib:format("No reservations for bot ~s", [BotId]);
                {ok, Positions} ->
                    io_lib:format("Bot ~s reservations: ~p", [BotId, Positions]);
                {error, Reason} ->
                    io_lib:format("ERROR: ~p", [Reason])
            end;
            
        {clear_reservations} ->
            case reservation_model:clear_all_reservations() of
                ok -> "All reservations cleared";
                {error, Reason} -> io_lib:format("ERROR: ~p", [Reason])
            end;
            
        {complete_movement, BotId} ->
            io:format("CLIENT: Bot ~s reached destination! Completing movement...~n", [BotId]),
            case bot_manager:complete_bot_movement(BotId) of
                ok -> 
                    io:format("CLIENT: Bot ~s movement completion successful~n", [BotId]),
                    io_lib:format("Bot ~s movement completed, reservations released", [BotId]);
                {error, Reason} -> 
                    io:format("CLIENT: Bot ~s movement completion failed: ~p~n", [BotId, Reason]),
                    io_lib:format("ERROR: ~p", [Reason])
            end;
        
        {move_to_with_path, BotId, {X, Y}, Path} ->
            % This is always from server - display with path info
            put(server_command_flag, false),  % Reset flag
            io:format("CLIENT: Received movement instruction for bot ~s to ~p~n", [BotId, {X, Y}]),
            %io:format("CLIENT: Path to follow: ~p~n", [Path]),
            io:format("CLIENT: Total steps: ~w~n", [length(Path)]),
            io:format("CLIENT: Bot ~s starting movement to destination ~p~n", [BotId, {X, Y}]),
            FormattedPath = format_path(Path),
            io_lib:format("OK: Bot ~s moving to ~p via ~s (Server command received)", [BotId, {X, Y}, FormattedPath]);
        
        {destination_reached, BotId} ->
            io:format("CLIENT: Bot ~s reached destination!~n", [BotId]),
            case bot_manager:complete_bot_movement(BotId) of
                ok -> 
                    io:format("CLIENT: Bot ~s movement completion successful~n", [BotId]),
                    io_lib:format("OK: Bot ~s reached destination, reservations released", [BotId]);
                {error, Reason} -> 
                    io:format("CLIENT: Bot ~s movement completion failed: ~p~n", [BotId, Reason]),
                    io_lib:format("ERROR: ~p", [Reason])
            end;
        
        ping -> 
            "pong";
        
        {error, Reason} -> 
            io_lib:format("ERROR: Invalid command - ~p", [Reason]);
        
        unknown -> 
            "ERROR: Unknown command"
    end.

%% Parse different command formats
parse_command("ping") -> ping;
parse_command("get_empty_positions") -> {get_empty_positions};

%% Parse: plan_path 1,2 5,6
parse_command("plan_path " ++ Rest) ->
    try
        [Start, End] = string:tokens(Rest, " "),
        [SX, SY] = [list_to_integer(X) || X <- string:tokens(Start, ",")],
        [DX, DY] = [list_to_integer(X) || X <- string:tokens(End, ",")],
        {plan_path, {SX, SY}, {DX, DY}}
    catch
        _:_ -> {error, "Invalid plan_path format. Use: plan_path 1,2 5,6"}
    end;

%% Parse: register_bot bot1 3,4
parse_command("register_bot " ++ Rest) ->
    try
        [BotId, Pos] = string:tokens(Rest, " "),
        [X, Y] = [list_to_integer(P) || P <- string:tokens(Pos, ",")],
        {register_bot, BotId, {X, Y}}
    catch
        _:_ -> {error, "Invalid register_bot format. Use: register_bot bot1 3,4"}
    end;

%% Parse: move_bot bot1 5,7
parse_command("move_bot " ++ Rest) ->
    try
        [BotId, Pos] = string:tokens(Rest, " "),
        [X, Y] = [list_to_integer(P) || P <- string:tokens(Pos, ",")],
        {move_bot, BotId, {X, Y}}
    catch
        _:_ -> {error, "Invalid move_bot format. Use: move_bot bot1 5,7"}
    end;

%% Parse: assign_path bot1 1,2 8,9
parse_command("assign_path " ++ Rest) ->
    try
        [BotId, Start, End] = string:tokens(Rest, " "),
        [SX, SY] = [list_to_integer(X) || X <- string:tokens(Start, ",")],
        [DX, DY] = [list_to_integer(X) || X <- string:tokens(End, ",")],
        {assign_path, BotId, {SX, SY}, {DX, DY}}
    catch
        _:_ -> {error, "Invalid assign_path format. Use: assign_path bot1 1,2 8,9"}
    end;

%% Parse: move_to bot1 5,7
parse_command("move_to " ++ Rest) ->
    try
        [BotId, CoordStr] = string:tokens(Rest, " "),
        [XStr, YStr] = string:tokens(CoordStr, ","),
        X = list_to_integer(XStr),
        Y = list_to_integer(YStr),
        {move_to, BotId, {X, Y}}
    catch
        _:_ -> {error, "Invalid move_to format. Use: move_to bot1 5,7"}
    end;

parse_command("move_to_with_path " ++ Rest) ->
    try
        % Parse: "move_to_with_path bot1 5,7 [{2,1},{3,1},{4,1}...]"
        Parts = string:split(Rest, " ", all),
        [BotId, CoordStr | PathParts] = Parts,
        [XStr, YStr] = string:tokens(CoordStr, ","),
        X = list_to_integer(XStr),
        Y = list_to_integer(YStr),
        PathStr = string:join(PathParts, " "),
        {ok, Tokens, _} = erl_scan:string(PathStr ++ "."),
        {ok, Path} = erl_parse:parse_term(Tokens),
        {move_to_with_path, BotId, {X, Y}, Path}
    catch
        _:_ -> {error, "Invalid move_to_with_path format"}
    end;

%% Parse: get_bot bot1
parse_command("get_bot " ++ BotId) ->
    {get_bot, string:trim(BotId)};

parse_command("get_all_bots") -> {get_all_bots};

%% Reservation commands
parse_command("get_reservations") -> {get_reservations};
parse_command("get_bot_reservations " ++ BotId) -> {get_bot_reservations, string:trim(BotId)};
parse_command("clear_reservations") -> {clear_reservations};
parse_command("complete_movement " ++ BotId) -> {complete_movement, string:trim(BotId)};
parse_command("destination_reached " ++ BotId) -> {destination_reached, string:trim(BotId)};
parse_command("ping") -> ping;
parse_command(_) -> {error, "Unknown command"}.

%% Format path coordinates nicely
format_path([]) -> "empty";
format_path(Path) ->
    Steps = [io_lib:format("(~w,~w)", [X,Y]) || {X,Y} <- Path],
    string:join(Steps, " -> ").

%% Helper function to format reservations
format_reservations(Reservations) ->
    Lines = [io_lib:format("~p reserved by ~s (path: ~s)", 
                          [R#reservation.position, R#reservation.bot_id, R#reservation.path_id]) 
             || R <- Reservations],
    string:join(Lines, "\n").
