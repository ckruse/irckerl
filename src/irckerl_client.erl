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

-module(irckerl_client).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(gen_fsm).

-include("irckerl.hrl").

-record(state, {nick, normalized_nick, socket, settings, user_info, no_spoof, the_timer, last_activity, ping_sent}).

%% entry points
-export([start/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: registering_nick -> registering_user -> ready
-export([registering_nick/2, registering_user/2, ready/2]).

start([Settings, Socket]) ->
    gen_fsm:start(?MODULE, [Settings, Socket], [{debug,trace}]).

start_link([Settings, Socket]) ->
    gen_fsm:start_link(?MODULE, [Settings, Socket], []).



init([Settings, Client]) ->
    process_flag(trap_exit, true),
    case gen_server:call(irckerl,{register_client, self()}) of
        ok ->
            case set_timer(Settings) of
                {ok, Timer} ->
                    {ok, registering_nick, #state{nick=undefined,socket=Client, settings=Settings, no_spoof = get_no_spoof(32), last_activity = erlang:now(), the_timer = Timer}};

                {error, Reason} ->
                    error_logger:error_msg("error when registering as a client: ~p~n",[Reason]),
                    {error, {timer_failed,Reason}};
                Other ->
                    error_logger:error_msg("error when registering as a client: ~p~n",[Other]),
                    {error, {other, Other}}
            end;

        Other ->
            error_logger:error_msg("error when registering as a client: ~p~n",[Other]),
            {error, not_accepted}
    end.


get_no_spoof(I) when I > 0 ->
    [random:uniform(57) + 64] ++ get_no_spoof(I-1);
get_no_spoof(_) ->
    [].



handle_info({tcp_closed, Socket},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0, quit),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0, quit),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    Line = trim:trim(Data),

    io:format("R: ~p~n", [Line]),

    gen_fsm:send_event(self(), {received, Line}),
    {next_state, SName, State};

handle_info(ping, SName, State) ->
    gen_fsm:send_event(self(),ping),
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



terminate(_Reason, _StateName, _TheState) ->
    io:format("terminating~n"),
    ok.



%%%%
%%%% states
%%%%


registering_nick({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, "NICK",[Nick]} ->
            case irckerl_parser:valid_nick(Nick) of
                valid ->
                    NormNick = irckerl_parser:normalize_nick(Nick),
                    case send_server({reserve_nick,Nick,NormNick,self()}) of
                        ok ->
                            NState = reset_timer(State),
                            {next_state, registering_user, NState#state{nick = Nick, normalized_nick = NormNick}};

                        _Other ->
                            ?DEBUG("Error: nick could not be reserved"),
                            send(State,"433",[Nick, " :Nick already in use, choose another one"]),
                            {next_state, registering_nick, reset_timer(State)}
                    end;

                invalid ->
                    ?DEBUG("Error: invalid nick name ~p",[Nick]),
                    send(State,"432",[Nick," :Error in nick name, choose another one"]),
                    {next_state, registering_nick, reset_timer(State)}
            end;

        {ok, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, registering_nick, State};

        {ok, "PONG", [Ref]} ->
            case Ref == State#state.no_spoof of
                true ->
                    {next_state, registering_nick, (reset_timer(State))#state{ping_sent=false,no_spoof=get_no_spoof(32)}};
                _Other ->
                    {next_state, registering_nick, reset_timer(State)}
            end;

        {ok, Cmd, _} ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", [Cmd, " :Register first!"]),
            {next_state, registering_nick, reset_timer(State)};

        _Other ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", [Data, " :Register first!"]),
            {next_state, registering_nick, reset_timer(State)}
    end;

registering_nick(quit,State) ->
    {stop, shutdown, State};
registering_nick(ping, State) ->
    {next_state, registering_nick, try_ping(prenick,State)};
registering_nick(What,State) ->
    io:format("wtf? ~p~n",[What]),
    {next_state, registering_nick, State}.



registering_user({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, "USER", [Username,Hostname,Servername,Realname]} ->
            NState = State#state{user_info = [{given,[{user,Username},{host,Hostname},{server,Servername},{realname,Realname}]}]},
            send_first_messages(NState),
            {next_state, ready, reset_timer(NState)};

        {ok, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, registering_user, reset_timer(State)};

        {ok, "PONG", [Receiver]} ->
            handle_pong(Receiver,State);

        {ok, Cmd, _} ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", [Cmd, " :Register first!"]),
            {next_state, registering_user, reset_timer(State)};
        _Other ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", [Data, " :Register first!"]),
            {next_state, registering_user, reset_timer(State)}
    end;

registering_user(quit, State) ->
    {next_state, registering_user, State};
registering_user(ping, State) ->
    {next_state, registering_user, try_ping(State)};
registering_user(What,State) ->
    io:format("wtf? ~p~n",[What]),
    {next_state, registering_user, State}.




ready(ping, State) ->
    {next_state, registering_user, try_ping(State)};
ready(quit, State) ->
    {next_state, registering_nick, State};
ready(What, TheState) ->
    {next_state, ready, TheState}.


%%%
%%% internal
%%%

send(State, Code, Data) ->
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    send(State#state.socket, [":", Host, " ", Code, " ", Data, "\r\n"]).

send(Sock,Msg) ->
    io:format("S: ~p~n",[Msg]),
    gen_tcp:send(Sock,Msg).


send_server(What) ->
    gen_server:call(irckerl,What).


set_timer(Settings) ->
    timer:send_after(proplists:get_value(pingfreq,Settings,10) * 1000,ping).


reset_timer(State) ->
    case timer:cancel(State#state.the_timer) of
        {ok, cancel} ->
            case set_timer(State#state.settings) of
                {ok, TRef} ->
                    State#state{the_timer = TRef, last_activity = erlang:now()};

                {error, Reason} ->
                    error_logger:error_msg("Error creating timer: ~p",[Reason]),
                    State#state{last_activity = erlang:now()}
            end;

        {error, Reason} ->
            error_logger:error_msg("Error canceling timer: ~p",[Reason]),
            State#state{last_activity = erlang:now()}
    end.


try_ping(State) ->
    try_ping(State, proplists:get_value(hostname,State#state.settings,"localhost")).
try_ping(prenick,State) ->
    try_ping(State, State#state.no_spoof);

try_ping(State, What) ->
    case State#state.ping_sent of
        true ->
            send(State#state.socket,["ERROR :Connection timed out\r\n"]),
            gen_fsm:send_event_after(0,quit),
            NState = State#state{the_timer=undefined};

        _Other ->
            send(State#state.socket,["PING :", What, "\r\n"]),
            case set_timer(State#state.settings) of
                {ok, TRef} ->
                    NState = State#state{the_timer=TRef,ping_sent=true};
                {error,Reason} ->
                    error_logger:error_msg("Error creating timer: ~p",[Reason]),
                    NState = State#state{the_timer=undefined,ping_sent=true}
            end
    end,
    NState#state{last_activity=erlang:now()}.

handle_pong(Receiver,State) ->
    case Receiver == proplists:get_value(hostname,State#state.settings,"localhost") of
        true ->
            {next_state, registering_nick, (reset_timer(State))#state{ping_sent=false}};
        _Other ->
            {next_state, registering_nick, reset_timer(State)}
    end.


send_first_messages(_State) ->
    ok.


% eof
