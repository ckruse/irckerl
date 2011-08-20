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

-record(state, {nick, normalized_nick, socket, timer, settings}).

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
            {ok, registering_nick, #state{nick=undefined,socket=Client,timer=0, settings=Settings}};
        Other ->
            io:format("error: ~p~n",[Other]),
            {error, not_accepted}
    end.



handle_info({tcp_closed, Socket},SName,State) ->
    io:format("closed: client exited~n"),
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0,self(), quit),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0,self(), quit),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    [Line|_] = re:split(Data, "\r\n"),

    io:format("R: ~p~n", [Line]),

    gen_fsm:send_event(self(), {received, Line}),
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

send(State, Code, Data) ->
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    send(State#state.socket, [":", Host, " ", Code, " ", Data, "\r\n"]).

send(Sock,Msg) ->
    io:format("S: ~p~n",[Msg]),
    gen_tcp:send(Sock,Msg).


registering_nick({received, Data}, State) ->
    case irckerl_parser:parse_client(Data) of
        {ok, "NICK",[Nick]} ->
            case irckerl_parser:valid_nick(Nick) of
                valid ->
                    NormNick = irckerl_parser:normalize_nick(Nick),
                    {next_state, registering_user, State#state{nick = Nick, normalized_nick = NormNick}};

                invalid ->
                    error_logger:error_msg("Error: invalid nick name ~p",[Nick]),
                    send(State,"432",[Nick," :Error in nick name, choose another one"]),
                    {next_state, registering_nick, State}
            end;

        {ok, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, registering_nick, State};

        {ok, Cmd, _} ->
            error_logger:error_msg("Error: unexpected data: ~p~n",[data]),
            send(State, "451", [Cmd, " :Register first!"]),
            {next_state, registering_nick, State};

        _Other ->
            error_logger:error_msg("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", [Data, " :Register first!"]),
            {next_state, registering_nick, State}
    end;

registering_nick(quit,State) ->
    {stop, shutdown, State};

registering_nick(What,State) ->
    io:format("wtf? ~p~n",[What]),
    {next_state, registering_nick, State}.



registering_user(quit, State) ->
    {next_state, registering_nick, State};

registering_user(What, TheState) ->
    {next_state, registering_user, TheState}.



ready(quit, State) ->
    {next_state, registering_nick, State};
ready(What, TheState) ->
    {next_state, ready, TheState}.



% eof
