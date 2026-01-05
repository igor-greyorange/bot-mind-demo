%% TCP server that listens for incoming bot connections and handles communication
%% Accepts client connections on port 5555 and spawns session handlers for each bot
-module(bot_comm_listener).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) -> gen_server:start_link(?MODULE, Port, []).

init(Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port,[binary,{packet,line},{reuseaddr,true},{active,false}]),
    spawn(fun() -> accept_loop(ListenSock) end),
    io:format("bot_comm_listener listening on port ~p~n",[Port]),
    {ok, #{} }.

accept_loop(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            {ok, Pid} = bot_session:start(Socket),
            ok = gen_tcp:controlling_process(Socket, Pid),
            accept_loop(ListenSock);
        {error,_} -> ok
    end.

%% gen_server callbacks
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
