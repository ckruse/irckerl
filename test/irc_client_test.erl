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

% TODO: implement real tests!

-module(irc_client_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

connect_close_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),
    send(Sock, "QUIT"),
    receive
        {tcp_closed, _} ->
            ok
    after
        5000 -> throw({error, timeout})
    end,
    gen_tcp:close(Sock),
    stop(Pid).

nick_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    ?assertMatch(<<"PING :", _/binary>>, get_msg()),
    gen_tcp:close(Sock),
    stop(Pid).

user_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", Id]),
    send(Sock, "USER ckruse x y :Christian Kruse"),
    ?assertMatch(<<":localhost 001 Welcome", _/binary>>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).


connect() ->
    Pid = start([{port, 6668}, {interface, {127,0,0,1}}]),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 6668, [binary, {active, true}, {reuseaddr, true}, {packet, line}, {keepalive, true}]),
    {Pid, Sock}.

start(Settings) ->
    {ok, Pid} = irckerl_controller:start_link(Settings),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop).

send(Sock, Msg) ->
    gen_tcp:send(Sock, [Msg, "\015\012"]).

get_msg() ->
    receive
        {tcp, _, Data} ->
            Data
    after
        5000 ->
            throw({error, timeout})
    end.

% eof
