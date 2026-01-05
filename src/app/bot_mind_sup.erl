%% OTP supervisor that manages and monitors child processes in the bot system
%% Provides fault tolerance by restarting failed processes automatically
-module(bot_mind_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
    %% child spec
    ListenerChild = {bot_comm_listener,
                     {bot_comm_listener, start_link, [Port]},
                     permanent,
                     2000,
                     worker,
                     [bot_comm_listener]},
    {ok, {{one_for_one, 5, 10}, [ListenerChild]}}.
