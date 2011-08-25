%% Copyright (C) 2011 by Christian Kruse <cjk@wwwtech.de>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(irckerl).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(gen_server).

-include("irckerl.hrl").

-define(SERVER, ?MODULE).

-record(state, {max_clients = ?DEFAULT_MAX_CLIENTS,
                listen_socket = undefined, listen_port, listen_interface,
                listener_process, clients, settings, reserved_nicks,
                created, servers}).



% API
-export([start_link/1, start_link/4, stop/0]).

%-export([register_client/1, get_connected_client_count/0]).
%-export([send/1, send/2, terminate/0, terminate/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).



start_link([Settings]) ->
    Lim = proplists:get_value(limits,Settings,[]),
    Port = proplists:get_value(port,Settings,6667),
    Interface = proplists:get_value(interface, Settings, all),
    MaxClients = proplists:get_value(maxusers, Lim, ?DEFAULT_MAX_CLIENTS),
    start_link(Settings, Port,Interface, MaxClients).

start_link(Settings, Port, Interface, MaxClients) ->
    error_logger:info_msg("starting irckerl..."),

    case gen_server:start_link({local, ?SERVER}, ?MODULE, [Settings, Port, Interface, MaxClients], []) of
        {ok, Server} ->
            error_logger:info_msg("gen_server:start_link was successful"),
            {ok, Server};

        {error, {already_started, Server}} ->
            error_logger:info_msg("gen_server:start_link was error: already_started"),
            {ok, Server};

        {error, Reason} ->
            error_logger:error_msg("Error starting irckerl: ~w",[Reason]),
            {error, Reason}
    end.



init([Settings, Port, Interface, MaxClients]) ->
    process_flag(trap_exit, true),

    case catch gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}, {packet, line}, {keepalive, true}]) of
        {ok, Listener} ->
            LisProc = spawn_link(fun() ->
                                         process_flag(trap_exit, true),
                                         socket_listener(Listener, Settings)
                                 end),

            {ok, #state{
               listen_port = Port,
               listen_interface = Interface,
               listen_socket = Listener,
               listener_process = LisProc,
               max_clients = MaxClients,
               clients = [], settings = Settings,
               reserved_nicks = dict:new(),
               created = erlang:localtime(),
               servers = []
              }
            };

        Error ->
            {error, {listen_failed, Error}}
    end.



stop() ->
    gen_server:call(?SERVER, stop).






handle_call(stop, _, State = #state{listen_socket = Listener}) ->
    gen_tcp:close(Listener),
    {stop, stop_requested, State#state{listen_socket = undefined}};

handle_call(created, _, State = #state{created = Created}) ->
    {reply, {created, Created}, State};

handle_call(count_users, _, State = #state{clients = Clients}) ->
    {reply, {visible, length(Clients), invisible, 0}, State};

handle_call(count_servers, _, State = #state{servers = Servers}) ->
    {reply, {servers, length(Servers)}, State};

handle_call({register_client, _}, _, State = #state{max_clients = MaxClients, clients = Clients}) when length(Clients) >= MaxClients ->
    {reply, {error, max_connect}, State};

handle_call({register_client, ClientPid}, _, State = #state{clients = Clients}) ->
    erlang:monitor(process, ClientPid),
    {reply, ok, State#state{clients = Clients ++ [ClientPid]}};

handle_call({reserve_nick,Nick,NormNick,ByWhom}, _, State = #state{reserved_nicks = RNicks}) ->
    case dict:find(NormNick, RNicks) of
        {ok, _} ->
            {reply, nick_registered_already, State};
        _Other ->
            {reply, ok, State#state{reserved_nicks = dict:append(NormNick, {Nick, ByWhom}, RNicks)}}
    end;

handle_call({delete_nick,NormNick}, _, State = #state{reserved_nicks = RNicks}) ->
    case dict:find(NormNick, RNicks) of
        {ok, _} ->
            {reply, ok, State#state{reserved_nicks = dict:erase(NormNick,RNicks)}};
        _Other ->
            {reply, not_found, State}
    end;

handle_call(_, _, State) ->
    {reply, ok, State}.



handle_cast(_, State) ->
    {noreply, State}.



handle_info({'DOWN', _, process, ClientPid, _}, State = #state{listener_process = LProc, listen_socket = Listener, settings = Settings}) when LProc == ClientPid ->
    LisProc = spawn_listener(Listener, Settings),
    {noreply, State#state{listener_process = LisProc}};

% client left - remove PID from ist
handle_info({'DOWN', _, process, ClientPid, _}, State = #state{clients = Clients}) ->
    NClients = lists:filter(fun(X) when X =/= ClientPid -> true;
                               (_) -> false
                            end,Clients),

    {noreply, State#state{clients = NClients}};

handle_info({'EXIT', _ClientPid}, State = #state{listen_socket = Listener}) when Listener =/= undefined ->
    gen_tcp:close(Listener),
    {noreply, State#state{listen_socket = undefined}};


handle_info(_, State) ->
    {noreply, State}.




code_change(_, State, _) ->
    {ok, State}.



terminate(_, []) ->
    io:format("down with listener~n"),
    ok;
terminate(_, #state{listen_socket = Listener}) when Listener =/= undefined ->
    io:format("down with listener~n"),
    gen_tcp:close(Listener),
    ok.



%%%
%%% internal functions
%%%


spawn_listener(Listener, Settings) ->
    spawn_link(fun() ->
                       process_flag(trap_exit, true),
                       socket_listener(Listener, Settings)
               end).


socket_listener(Listener, Settings) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            case irckerl_client:start_link([Settings, Socket]) of
                {ok, ClientPid} ->
                    gen_tcp:controlling_process(Socket, ClientPid);
                _Other ->
                    error_logger:error_msg("error spawning client handler, closing client"),
                    gen_tcp:close(Socket)
            end,

            socket_listener(Listener, Settings);

        {error, Reason} ->
            error_logger:error_msg("Error in accept: ~p; listener",[Reason]),
            gen_tcp:close(Listener)
    end.



% eof
