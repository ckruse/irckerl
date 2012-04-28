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

garbage_test() ->
    {Pid, Sock} = connect(),

    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),
    send(Sock, "weufewofwefwfe"),
    ?assertMatch(<<":localhost WEUFEWOFWEFWFE 451 :Register first!\r\n">>, get_msg()),

    stop(Pid).

nick_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    ?assertMatch(<<"PING :", _/binary>>, get_msg()),
    gen_tcp:close(Sock),
    stop(Pid).

duplicate_nick_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),
    send(Sock, ["PONG :", trim:trim(Id)]),

    {ok, Sock1} = gen_tcp:connect({127,0,0,1}, 6668, [binary, {active, true}, {reuseaddr, true}, {packet, line}, {keepalive, true}]),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock1,"NICK cjk101010"),
    ?assertMatch(<<":localhost cjk101010 433 :Nick already in use, choose another one\r\n">>, get_msg()),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock),
    stop(Pid).

invalid_nick_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010!!"),
    ?assertMatch(<<":localhost cjk101010!! 432 :Error in nick name, choose another one\r\n">>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).

user_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", trim:trim(Id)]),

    send(Sock, "USER ckruse x y :Christian Kruse"),
    ?assertMatch(<<":localhost 001", _/binary>>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).

user_w_mode_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", trim:trim(Id)]),

    send(Sock, "USER ckruse 12 y :Christian Kruse"),
    ?assertMatch(<<":localhost 001", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 002", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 003", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 004", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 251", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 422", _/binary>>, get_msg()),
    ?assertMatch(<<":cjk101010 MODE cjk101010 :+iw">>, trim:trim(get_msg())),

    gen_tcp:close(Sock),
    stop(Pid).

user_w_unknown_mode_test() ->
    {Pid, Sock} = connect(),

    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", trim:trim(Id)]),

    send(Sock, "USER ckruse 2 y :Christian Kruse"),
    ?assertMatch(<<":localhost 001", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 002", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 003", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 004", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 251", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 422", _/binary>>, get_msg()),
    ?assertMatch(<<":cjk101010 MODE cjk101010 :+iwx">>, trim:trim(get_msg())),

    gen_tcp:close(Sock),
    stop(Pid).

invalid_user_test() ->
        {Pid, Sock} = connect(),

    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", trim:trim(Id)]),

    send(Sock, "USER ckruse@lala x y :Christian Kruse"),
    ?assertMatch(<<":localhost 461 cjk101010 :Invalid username\r\n">>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).


join_test() ->
    {Pid, Sock} = connect(),
    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,"NICK cjk101010"),
    <<"PING :", Id/binary>> = get_msg(),

    send(Sock, ["PONG :", trim:trim(Id)]),

    send(Sock, "USER ckruse x y :Christian Kruse"),
    ?assertMatch(<<":localhost 001", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 002", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 003", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 004", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 005", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 251", _/binary>>, get_msg()),
    ?assertMatch(<<":localhost 422", _/binary>>, get_msg()),
    ?assertMatch(<<":cjk101010 MODE", _/binary>>, get_msg()),

    send(Sock, "JOIN #selfhtml"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list">>, trim:trim(get_msg())),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN #selfhtml">>, trim:trim(get_msg())),

    send(Sock, "JOIN 0"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #selfhtml :Leaving all channels">>, trim:trim(get_msg())),

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
