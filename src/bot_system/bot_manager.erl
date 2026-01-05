%% High-level bot management operations including registration, task assignment, and movement coordination
%% Handles path planning, collision avoidance, and bot lifecycle management
-module(bot_manager).
-include("../data/models/schema.hrl").
-export([register_bot/3, get_bot/1, update_bot_pos/2, assign_task/2, 
         get_all_bots/0, get_bot_at_position/1, is_position_occupied/1,
         complete_bot_movement/1, server_move_bot/2, send_movement_command/3,
         demo_server_movements/0, start_server_shell/0]).

%% Register bot in Mnesia 
register_bot(BotId, SessionPid, Position) ->
    case bot_model:register_bot(BotId, SessionPid, Position) of
        {ok, _BotRecord} ->
            io:format("Bot ~s registered at position ~p~n", [BotId, Position]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Get bot using model layer
get_bot(BotId) ->
    bot_model:get_bot(BotId).

%% Update bot position using model layer
update_bot_pos(BotId, NewPosition) ->
    bot_model:update_bot_position(BotId, NewPosition).

%% Assign task to bot
assign_task(BotId, Task) ->
    case bot_model:get_bot(BotId) of
        {ok, BotRecord} ->
            CurrentPos = BotRecord#bot.position,
            
            % Release any existing reservations for this bot
            reservation_model:release_bot_reservations(BotId),
            
            case Task of
                {move, From, To} ->
                    handle_move_task(BotRecord, BotId, From, To, Task);
                {move_to, Dest} ->
                    handle_move_task(BotRecord, BotId, CurrentPos, Dest, Task);
                _ ->
                    {error, invalid_task}
            end;
        {error, not_found} ->
            {error, bot_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper function to handle move tasks with reservation
handle_move_task(_BotRecord, BotId, From, To, Task) ->
    io:format("Planning path for bot ~s from ~p to ~p~n", [BotId, From, To]),
    case path_planner:plan_path(From, To) of
        [] ->
            io:format("No path found from ~p to ~p~n", [From, To]),
            {error, no_path_found};
        Path ->
            io:format("Path planned: ~p~n", [Path]),
            io:format("Path length: ~w steps~n", [length(Path)]),
            
            % Try to reserve the path
            io:format("Attempting to reserve path for bot ~s...~n", [BotId]),
            case reservation_model:reserve_path(BotId, Path) of
                {ok, PathId} ->
                    io:format("Path reservation SUCCESS! PathID: ~s~n", [PathId]),
                    % Path successfully reserved, update bot status
                    case bot_model:update_bot_status(BotId, moving, Task) of
                        ok ->
                            {ok, Path, PathId};
                        {error, Reason} ->
                            % If update fails, release the reservation
                            reservation_model:release_path(PathId),
                            {error, Reason}
                    end;
                {error, {position_reserved, Position, ReservingBot}} ->
                    io:format("Path reservation FAILED! Position ~p already reserved by bot ~s~n", [Position, ReservingBot]),
                    {error, {path_blocked, Position, ReservingBot}};
                {error, Reason} ->
                    io:format("Path reservation ERROR: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

%% Complete bot movement and release reservations
complete_bot_movement(BotId) ->
    case bot_model:get_bot(BotId) of
        {ok, BotRecord} ->
            % Release path reservations
            case BotRecord#bot.path_id of
                undefined -> 
                    io:format("Bot ~s movement complete (no reservations to release)~n", [BotId]);
                PathId -> 
                    reservation_model:release_path(PathId),
                    io:format("Bot ~s reached destination! Path reservations released (PathID: ~s)~n", [BotId, PathId])
            end,
            
            % Update bot status using model layer
            case bot_model:update_bot_status(BotId, idle, none) of
                ok ->
                    io:format("Bot ~s status updated to IDLE~n", [BotId]),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            io:format("Bot ~s not found for completion~n", [BotId]),
            {error, bot_not_found};
        {error, Reason} ->
            io:format("Failed to complete bot ~s movement: ~p~n", [BotId, Reason]),
            {error, Reason}
    end.

handle_path_result(BotId, Pid, Path, Destination) ->
    case Path of
        [] -> 
            {error, no_path_found};
        _ ->
            send_path(Pid, Path),
            update_bot_pos(BotId, Destination),
            {ok, Path}
    end.

send_path(Pid, Path) ->
    PathStr = io_lib:format("path ~p", [Path]),
    bot_session:send(Pid, PathStr).

%% Get all bots using model layer
get_all_bots() ->
    bot_model:get_all_bots().

%% Get bot by position from Mnesia
get_bot_at_position({X, Y}) ->
    case get_all_bots() of
        {ok, AllBots} ->
            lists:filter(fun(#bot{position={BX, BY}}) -> 
                             BX =:= X andalso BY =:= Y 
                 end, AllBots);
        {error, _} ->
            []
    end.

%% Check if position is occupied by any bot
is_position_occupied({X, Y}) ->
    case get_bot_at_position({X, Y}) of
        [] -> false;
        _ -> true
    end.

%% Server-initiated bot movement
server_move_bot(BotId, Destination) ->
    case assign_task(BotId, {move_to, Destination}) of
        {ok, Path, _PathId} ->
            % Path planned successfully, now send movement command to bot
            send_movement_command(BotId, Destination, Path);
        {error, Reason} ->
            io:format("Failed to move bot ~s to ~p: ~p~n", [BotId, Destination, Reason]),
            {error, Reason}
    end.

%% Send movement command directly to bot session
send_movement_command(BotId, Destination, Path) ->
    case get_bot(BotId) of
        {ok, Bot} ->
            SessionPid = Bot#bot.session_pid,
            
            % Send movement command with path info: "move_to_with_path bot1 5,7 [{2,1},{3,1}...]"
            {X, Y} = Destination,
            PathStr = lists:flatten(io_lib:format("~p", [Path])),
            Command = lists:flatten(io_lib:format("move_to_with_path ~s ~w,~w ~s", [BotId, X, Y, PathStr])),
            SessionPid ! {server_command, Command},
            
            io:format("SERVER: Sending movement command to bot ~s~n", [BotId]),
            %io:format("Destination: ~p | Path: ~p~n", [Destination, Path]),
            {ok, Path};
        {error, Reason} ->
            io:format("Failed to send command to bot ~s: ~p~n", [BotId, Reason]),
            {error, Reason}
    end.

%% Demo function to show server-initiated movements
demo_server_movements() ->
    io:format("Starting demo of server-initiated bot movements...~n"),
    
    % Get all registered bots
    AllBots = get_all_bots(),
    
    case AllBots of
        [] ->
            io:format("No bots registered! Please connect some bots first.~n");
        _ ->
            % For each bot, send them to a random position
            lists:foreach(fun(Bot) ->
                BotId = Bot#bot.id,
                RandomDest = {rand:uniform(10), rand:uniform(10)},
                
                io:format("Demo: Moving bot ~s to ~p~n", [BotId, RandomDest]),
                server_move_bot(BotId, RandomDest),
                
                % Wait a bit between commands
                timer:sleep(2000)
            end, AllBots)
    end.

%% Interactive server shell for admin commands
start_server_shell() ->
    io:format("~n========================================~n"),
    io:format("    Server shell started...~n"),
    io:format("========================================~n"),
    
    shell_loop().

%% Command shell loop
shell_loop() ->
    io:format("server> "),
    case io:get_line("") of
        eof ->
            io:format("Goodbye!~n");
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            shell_loop();
        Line ->
            Command = string:trim(Line),
            handle_shell_command(Command),
            shell_loop()
    end.

%% Handle shell commands
handle_shell_command("quit") ->
    io:format("Exiting server shell...~n"),
    exit(normal);

handle_shell_command("help") ->
    io:format("Available Commands:~n"),
    io:format("  list_bots          - Show all registered bots~n"),
    io:format("  move_bot <id> <x>,<y>  - Move bot to position~n"),
    io:format("  bot_status <id>    - Show bot details~n"),
    io:format("  help               - Show this help~n"),
    io:format("  quit               - Exit shell~n");

handle_shell_command("list_bots") ->
    AllBots = get_all_bots(),
    case AllBots of
        [] ->
            io:format("No bots registered.~n");
        _ ->
            io:format("Registered Bots:~n"),
            lists:foreach(fun(Bot) ->
                io:format("  Bot ID: ~s, Position: ~p, Status: ~p~n", 
                         [Bot#bot.id, Bot#bot.position, Bot#bot.status])
            end, AllBots)
    end;

handle_shell_command("move_bot " ++ Rest) ->
    case parse_move_command(Rest) of
        {ok, BotId, Destination} ->
            io:format("Moving bot ~s to ~p...~n", [BotId, Destination]),
            case server_move_bot(BotId, Destination) of
                {ok, _Path} ->
                    io:format("Movement command sent successfully!~n");
                {error, Reason} ->
                    io:format("Failed to move bot: ~p~n", [Reason])
            end;
        error ->
            io:format("Invalid syntax. Use: move_bot <bot_id> <x>,<y>~n"),
            io:format("Example: move_bot bot1 5,7~n")
    end;

handle_shell_command("bot_status " ++ BotId) ->
    TrimmedBotId = string:trim(BotId),
    case get_bot(TrimmedBotId) of
        {ok, Bot} ->
            io:format("Bot Details:~n"),
            io:format("  ID: ~s~n", [Bot#bot.id]),
            io:format("  Position: ~p~n", [Bot#bot.position]),
            io:format("  Status: ~p~n", [Bot#bot.status]),
            io:format("  Task: ~p~n", [Bot#bot.task]),
            io:format("  Path ID: ~p~n", [Bot#bot.path_id]);
        {error, Reason} ->
            io:format("Bot not found: ~p~n", [Reason])
    end;

handle_shell_command("") ->
    % Empty command, do nothing
    ok;

handle_shell_command(Unknown) ->
    io:format("Unknown command: ~s~n", [Unknown]),
    io:format("Type 'help' for available commands.~n").

%% Parse move command: "bot1 5,7" -> {ok, "bot1", {5,7}}
parse_move_command(Command) ->
    try
        Parts = string:tokens(Command, " "),
        case Parts of
            [BotId, CoordStr] ->
                CoordParts = string:tokens(CoordStr, ","),
                case CoordParts of
                    [XStr, YStr] ->
                        X = list_to_integer(string:trim(XStr)),
                        Y = list_to_integer(string:trim(YStr)),
                        {ok, string:trim(BotId), {X, Y}};
                    _ ->
                        error
                end;
            _ ->
                error
        end
    catch
        _:_ -> error
    end.
