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

-module(irckerl_controller).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(gen_server).

-include("irckerl.hrl").

-import(irc_controller).

-define(SERVER, ?MODULE).

% API
-export([start_link/1, start_link/4, stop/0]).


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc Starts the application.
%start() ->
    %application:start(irckerl,permanent).

% @doc Stops the application.
stop() ->
    gen_server:call(?SERVER, stop).

-spec start_link(proplist()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link(Settings) ->
    Lim        = proplists:get_value(limits,Settings,[]),
    Port       = proplists:get_value(port,Settings,6667),
    Interface  = proplists:get_value(interface, Settings, all),
    MaxClients = proplists:get_value(maxusers, Lim, ?DEFAULT_MAX_CLIENTS),
    start_link(Settings, Port,Interface, MaxClients).

-spec start_link(proplist(), integer(), inet:ip_address() | string(), integer()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link(Settings, Port, Interface, MaxClients) ->
    ?INFO("starting irckerl..."),

    case gen_server:start_link({local, ?SERVER}, ?MODULE, {Settings, Port, Interface, MaxClients}, [{debug, [trace]}]) of
        {ok, Server} ->
            ?INFO("gen_server:start_link was successful"),
            {ok, Server};

        {error, {already_started, Server}} ->
            ?INFO("gen_server:start_link was error: already_started"),
            {ok, Server};

        {error, Reason} ->
            ?ERROR("Error starting irckerl: ~p",[Reason]),
            {error, Reason}
    end.

-spec init({proplist(), integer(), inet:ip_address() | string(), integer()}) -> {ok, #controller_state{}} | {error, {listen_failed, _}}.
init({Settings, Port, Interface, MaxClients}) ->
    process_flag(trap_exit, true),

    case catch gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}, {packet, line}, {keepalive, true}]) of
        {ok, Listener} ->
            LisProc = spawn_link(fun() ->
                                         process_flag(trap_exit, true),
                                         socket_listener(Listener, Settings)
                                 end),

            {ok, #controller_state{
               listen_port      = Port,
               listen_interface = Interface,
               listen_socket    = Listener,
               listener_process = LisProc,
               max_clients      = MaxClients,
               settings         = Settings,
               reserved_nicks   = dict:new(),
               created          = erlang:localtime(),
               channels         = dict:new()
              }
            };

        Error ->
            {error, {listen_failed, Error}}
    end.




-spec handle_call(term(), _, #controller_state{}) -> {atom(), term(), #controller_state{}}.
handle_call(stop, _, State = #controller_state{listen_socket = Listener}) ->
    gen_tcp:close(Listener),
    {stop, normal, ok, State#controller_state{listen_socket = undefined}};

handle_call(created, _, State = #controller_state{created = Created}) ->
    {reply, {created, Created}, State};

handle_call(count_users, _, State = #controller_state{clients = Clients}) ->
    {reply, {visible, length(Clients), invisible, 0}, State};

handle_call(count_servers, _, State = #controller_state{servers = Servers}) ->
    {reply, {servers, length(Servers)}, State};

handle_call({register_client, _}, _, State = #controller_state{max_clients = MaxClients, clients = Clients}) when length(Clients) >= MaxClients ->
    {reply, {error, max_connect}, State};

handle_call({register_client, ClientPid}, _, State = #controller_state{clients = Clients}) ->
    erlang:monitor(process, ClientPid),
    {reply, ok, State#controller_state{clients = Clients ++ [#user{pid=ClientPid}]}};

handle_call({choose_nick,Nick,NormNick,User}, _, State) ->
    irc_controller:choose_nick(State, Nick, NormNick, User);

handle_call({join, Channel, User, Pass}, _, State) ->
    irc_controller:join(State, Channel, User, Pass);

handle_call({get_channel, Channel}, _, State) ->
    irc_controller:get_channel(State, Channel);

handle_call({get_user, Nick}, _, State) ->
    irc_controller:get_user(State, Nick);


handle_call(Call, _, State) ->
    ?DEBUG("handle_call(~p): unknown", [Call]),
    {reply, ok, State}.


-spec handle_cast(term(), #controller_state{}) -> {noreply, #controller_state{}}.
handle_cast({delete_nick,NormNick}, State) ->
    irc_controller:delete_nick(State, NormNick);

handle_cast(Cast, State) ->
    ?DEBUG("handle_cast(~p): unknown", [Cast]),
    {noreply, State}.


-spec handle_info(term(), #controller_state{}) -> {noreply, #controller_state{}}.
handle_info({'DOWN', _, process, ClientPid, _}, State = #controller_state{listener_process = LProc, listen_socket = Listener, settings = Settings}) when LProc == ClientPid ->
    LisProc = spawn_listener(Listener, Settings),
    {noreply, State#controller_state{listener_process = LisProc}};

% client left - remove PID from list TODO: remove also from channels
handle_info({'DOWN', _, process, ClientPid, _}, State = #controller_state{clients = Clients, reserved_nicks = RNicks}) ->
    [User] = lists:filter(fun(_ = #user{pid=X}) when X == ClientPid -> true;
                             (_) -> false
                          end,Clients),
    NClients = lists:filter(fun(_ = #user{pid=X}) when X =/= ClientPid -> true;
                               (_) -> false
                            end,Clients),

    {noreply, State#controller_state{clients = NClients, reserved_nicks = dict:erase(User#user.normalized_nick,RNicks)}};

handle_info({'EXIT', _ClientPid}, State = #controller_state{listen_socket = Listener}) when Listener =/= undefined ->
    gen_tcp:close(Listener),
    {noreply, State#controller_state{listen_socket = undefined}};


handle_info(Info, State) ->
    ?DEBUG("handle_info(~p): unknown", [Info]),
    {noreply, State}.



-spec code_change(term(), #controller_state{}, term()) -> {ok, #controller_state{}}.
code_change(_, State, _) ->
    {ok, State}.


-spec terminate(_, #controller_state{}) -> ok.
terminate(_, #controller_state{listen_socket = Listener}) when Listener =/= undefined ->
    ?DEBUG("down with listener~n"),
    gen_tcp:close(Listener),
    ok;

terminate(_, _) ->
    ?DEBUG("down with listener~n"),
    ok.




%%%
%%% internal functions
%%%

-spec spawn_listener(inet:socket(), proplist()) -> pid().
spawn_listener(Listener, Settings) ->
    spawn_link(fun() ->
                       process_flag(trap_exit, true),
                       socket_listener(Listener, Settings)
               end).

-spec socket_listener(inet:socket(), proplist()) -> any().
socket_listener(Listener, Settings) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            case irckerl_client:start_link({Settings, Socket}) of
                {ok, ClientPid} ->
                    gen_tcp:controlling_process(Socket, ClientPid);
                _ ->
                    ?ERROR("error spawning client handler, closing client"),
                    gen_tcp:close(Socket)
            end,

            socket_listener(Listener, Settings);

        {error, Reason} ->
            ?ERROR("Error in accept: ~p; listener",[Reason]),
            gen_tcp:close(Listener)
    end.



% eof
