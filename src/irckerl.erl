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

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_CLIENTS, 2048).
-define(TIMEOUT, 180000).

-record(state, {connected_clients = 0, max_clients = ?DEFAULT_MAX_CLIENTS,
                listen_socket = undefined, listen_port, listen_interface, clients}).


% API
-export([start_link/1, start_link/4, stop/0]).

%-export([register_client/1, get_connected_client_count/0]).
%-export([send/1, send/2, terminate/0, terminate/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).



start_link([Settings]) ->
    Port = proplists:get_value(port,Settings,6667),
    Interface = proplists:get_value(interface, Settings, all),
    MaxClients = proplists:get_value(max_clients, Settings, ?DEFAULT_MAX_CLIENTS),
    start_link(Settings, Port,Interface, MaxClients).

start_link(Settings, Port, Interface, MaxClients) ->
    error_logger:info_msg("starting irckerl..."),

    case gen_server:start_link({local, ?SERVER}, ?MODULE, [Settings, Port, Interface, MaxClients], []) of
        {ok, Server} ->
            error_logger:info_msg("gen_server:start_link was successful"),
            %Server ! {listen},
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
            spawn_link(fun() -> socket_listener(Listener, Settings) end),
            {ok, #state{listen_port = Port, listen_interface = Interface, listen_socket = Listener, max_clients = MaxClients, clients = []}};
        Error ->
            {error, {listen_failed, Error}}
    end.



stop() ->
    gen_server:call(?SERVER, stop).






handle_call(stop, _, State = #state{listen_socket = Listener}) ->
    gen_tcp:close(Listener),
    {stop, stop_requested, State#state{listen_socket = undefined}};

handle_call({register_client, _}, _, State = #state{connected_clients = ConnectedClients, max_clients = MaxClients}) when ConnectedClients >= MaxClients ->
    {reply, {error, max_connect}, State};

handle_call({register_client, ClientPid}, _, State = #state{connected_clients = ConnectedClients, clients = Clients}) ->
    erlang:monitor(process, ClientPid),
    {reply, ok, State#state{connected_clients = ConnectedClients + 1, clients = Clients ++ [ClientPid]}};


handle_call(_, _, State) ->
    {reply, ok, State}.



handle_cast(_, State) ->
    {noreply, State}.



% client left - remove PID from ist
handle_info({'DOWN', _, process, ClientPid, _}, State = #state{connected_clients = ConnectedClients, clients = Clients}) ->
    {noreply, State#state{connected_clients = ConnectedClients - 1, clients = lists:delete(ClientPid,Clients)}};

handle_info({'EXIT', _, _}, State = #state{listen_socket = Listener}) when Listener =/= undefined ->
    gen_tcp:close(Listener),
    {noreply, State#state{listen_socket = undefined}};

handle_info(_, State) ->
    {noreply, State}.




code_change(_, State, _) ->
    {ok, State}.



terminate(_, []) ->
    ok;
terminate(_, #state{listen_socket = Listener}) when Listener =/= undefined ->
    gen_tcp:close(Listener),
    ok.



%%%
%%% internal functions
%%%

socket_listener(Listener, Settings) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            %% ClientPid = spawn(fun() ->
            %%                           case gen_server:call(?SERVER, {register_client, self()}) of
            %%                               ok ->
            %%                                   {ok, Pid} = irckerl_client:start_link([Settings,Socket,self()]),
            %%                                   handle_client(Socket,Pid);

            %%                               E  -> error_reporter:info_msg("Client connect disallowed ~p~n", [E])
            %%                           end
            %%                   end),

            {ok, ClientPid} = irckerl_client:start_link([Settings, Socket]),
            gen_tcp:controlling_process(Socket, ClientPid),
            socket_listener(Listener, Settings);

        {error, closed} ->
            gen_tcp:close(Listener)
    end.


%% handle_client(Socket, Pid) ->
%%     receive
%%         {tcp_closed, Socket} ->
%%             gen_tcp:close(Socket),
%%             gen_fsm:send_event(Pid, quit);

%%         {tcp_error, Socket, _} ->
%%             gen_tcp:close(Socket),
%%             gen_fsm:send_event(Pid, quit);

%%         {tcp, Socket, Data} ->
%%             [Line|_] = re:split(Data, "\r\n"),

%%             io:format("R: ~p~n", [Line]),

%%             gen_fsm:send_event(Pid, {received, Data}),
%%             handle_client(Socket,Pid);

%%         {send, Data} ->
%%             io:format("S: ~p~n", [Data]),
%%             ok = gen_tcp:send(Socket, [Data, "\r\n"]),
%%             handle_client(Socket,Pid);

%%         quit ->
%%             gen_tcp:close(Socket)
%%     end.


% eof
