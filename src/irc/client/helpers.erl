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

-module(irc.client.helpers).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-export([send/4, send/3, send/2, send_server/1, cast_server/1]).

-include("irckerl.hrl").
-include("umodes.hrl").
-include("cmodes.hrl").

-import(proplists).
-import(io).
-import(gen_server).
-import(gen_tcp).

-spec send(#client_state{}, string(), string(), any()) -> ok | {error, inet:posix()}.
send(State, To, Code, Data) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    send(State#client_state.socket, [":", Host, " ", Code, " ", To, " ", Data, "\r\n"]).

-spec send(#client_state{}, string(), any()) -> ok | {error, inet:posix()}.
send(State, Code, Data) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    send(State#client_state.socket, [":", Host, " ", Code, " ", State#client_state.user#user.nick, " ", Data, "\r\n"]).

-spec send(#client_state{} | inet:socket(), any()) -> ok | {error, inet:posix()}.
send(State, Data) when is_tuple(State) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    send(State#client_state.socket, [":", Host, " ", Data, "\r\n"]);

send(Sock, Msg) ->
    ?DEBUG("S: ~p", [Msg]),
    gen_tcp:send(Sock, Msg).

-spec send_server(term()) -> term().
send_server(What) ->
    gen_server:call(irckerl_app, What).

-spec cast_server(term()) -> ok.
cast_server(What) ->
    gen_server:cast(irckerl_app, What).


%% eof
