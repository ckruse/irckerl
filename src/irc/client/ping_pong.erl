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

-module(irc.client.ping_pong).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-export([set_timer/1, reset_timer/1, try_ping/1, try_ping/2]).

-import(timer).
-import(proplists).
-import(gen_fsm).

-import(irc.client.helpers).

-include("../../irckerl.hrl").
-include("../../umodes.hrl").
-include("../../cmodes.hrl").

set_timer(Settings) ->
    timer:send_after(proplists:get_value(pingfreq, Settings, 10) * 1000, ping).

reset_timer(State) ->
    case timer:cancel(State#client_state.the_timer) of
        {ok, cancel} ->
            case set_timer(State#client_state.settings) of
                {ok, TRef} ->
                    State#client_state{the_timer = TRef, last_activity = erlang:now()};

                {error, Reason} ->
                    ?ERROR("Error creating timer: ~p", [Reason]),
                    State#client_state{last_activity = erlang:now()}
            end;

        {error, Reason} ->
            ?ERROR("Error canceling timer: ~p", [Reason]),
            State#client_state{last_activity = erlang:now()}
    end.


try_ping(State) ->
    try_ping(State, proplists:get_value(hostname, State#client_state.settings, "localhost")).
try_ping(prenick, State) ->
    try_ping(State, State#client_state.no_spoof);

try_ping(State, What) ->
    case State#client_state.ping_sent of
        true ->
            helpers:send(State#client_state.socket, ["ERROR :Connection timed out\r\n"]),
            gen_fsm:send_event(self(), quit),
            NState = State#client_state{the_timer=undefined};

        _ ->
            helpers:send(State#client_state.socket, ["PING :", What, "\r\n"]),
            case set_timer(State#client_state.settings) of
                {ok, TRef} ->
                    NState = State#client_state{the_timer=TRef, ping_sent=true};
                {error, Reason} ->
                    ?ERROR("Error creating timer: ~p", [Reason]),
                    NState = State#client_state{the_timer=undefined, ping_sent=true}
            end
    end,
    NState#client_state{last_activity=erlang:now()}.



%% eof
