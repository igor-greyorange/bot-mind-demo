%% Main application entry point that starts Mnesia database and initializes server components
%% Handles application startup, database setup, and launches the TCP communication listener
-module(bot_mind_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type,_Args) ->
    ok = mnesia:start(),
        %% Initialize database and grid
    database_setup:setup_database(),
    Port = 5555,
    {ok, CommPid} = bot_comm_listener:start_link(Port),
    io:format("Server ready!~n"),
    
    % Start interactive shell in a separate process
    spawn(fun() ->
        timer:sleep(2000), % Wait 2 seconds for server to fully start
        bot_manager:start_server_shell()
    end),
    
    {ok, CommPid}.

stop(_State) -> ok.
