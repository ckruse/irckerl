-module(irckerl_client).
-author("Christian Kruse <cjk@wwwtech.de>").

-behaviour(gen_fsm).

-record(state, {nick, socket, timer, settings}).

%% entry points
-export([start/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: registering_nick -> registering_user -> ready
-export([registering_nick/2, registering_user/2, ready/2]).

start([Settings, Socket]) ->
    gen_fsm:start(?MODULE, [Settings, Socket], []).

start_link([Settings, Socket]) ->
    gen_fsm:start_link(?MODULE, [Settings, Socket], []).



init([Settings, Client]) ->
    process_flag(trap_exit, true),
    {ok, registering_nick, #state{nick=undefined,socket=Client,timer=0, settings=Settings}}.



handle_info({tcp_closed, Socket},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), quit),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), quit),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    [Line|_] = re:split(Data, "\r\n"),

    io:format("R: ~p~n", [Line]),

    gen_fsm:send_event(self(), {received, Data}),
    {next_state, SName, State};

handle_info(Info, SName, State) ->
    io:format("handle_info(~p, ~p, ~p)~n",[Info, SName, State]),
    {next_state, SName, State}.



handle_event(Ev, StateName, TheState) ->
    io:format("handle_event(~p, ~p, ~p)~n",[Ev,StateName,TheState]),
    {stop, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.



handle_sync_event(Ev, From, StateName, TheState) ->
    io:format("handle_sync_event(~p, ~p, ~p, ~p)~n",[Ev, From, StateName, TheState]),
    {stop, "WTF?! Don't use gen_fsm:sync_send_all_state_event, fucker!"}.



code_change(_, Name, State, _) ->
    {ok, Name, State}.



terminate(_Reason, _StateName, _TheState) -> ok.



%%%%
%%%% states
%%%%


registering_nick({received, <<"NICK ", Nick/binary>>}, TheState) ->
    io:format("Nick: ~p~n",[Nick]),
    {next_state, registering_nick, TheState};

registering_nick({received, Data}, TheState) ->
    error_logger:error_msg("Error: unexpected data: ~p~n",[Data]),
    %TheState#state.parent ! {error_msg, 451, <<"">>},
    {next_state, registering_nick, TheState}.

registering_user(What, TheState) ->
    {next_state, registering_user, TheState}.


ready(What, TheState) ->
    {next_state, ready, TheState}.


% eof
