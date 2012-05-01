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
    prelude(Sock),

    send(Sock, "JOIN #selfhtml"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list">>, trim:trim(get_msg())),

    send(Sock, "JOIN 0"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #selfhtml :Leaving all channels">>, trim:trim(get_msg())),

    gen_tcp:close(Sock),
    stop(Pid).

part_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "JOIN #selfhtml"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list">>, trim:trim(get_msg())),

    send(Sock, "PART #selfhtml"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #selfhtml", _/binary>>, get_msg()),

    send(Sock, "JOIN #selfhtml,#lala"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml\r\n">>, get_msg()),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010 \r\n">>, get_msg()),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list\r\n">>, get_msg()),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#lala\r\n">>, get_msg()),
    ?assertMatch(<<":localhost 353 cjk101010 = #lala :cjk101010 \r\n">>, get_msg()),
    ?assertMatch(<<":localhost 366 cjk101010 #lala :End of NAMES list\r\n">>, get_msg()),

    send(Sock, "PART #selfhtml,#lala"),
    Msg = get_msg(),
    io:format("msg: ~p~n", [Msg]),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #selfhtml", _/binary>>, Msg),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #lala", _/binary>>, get_msg()),

    send(Sock, "JOIN #selfhtml,#lala"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml\r\n">>, get_msg()),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010 \r\n">>, get_msg()),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list\r\n">>, get_msg()),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#lala\r\n">>, get_msg()),
    ?assertMatch(<<":localhost 353 cjk101010 = #lala :cjk101010 \r\n">>, get_msg()),
    ?assertMatch(<<":localhost 366 cjk101010 #lala :End of NAMES list\r\n">>, get_msg()),

    send(Sock, "PART #selfhtml,#lala :just a reason"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #selfhtml :just a reason\r\n">>, get_msg()),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " PART #lala :just a reason\r\n">>, get_msg()),


    gen_tcp:close(Sock),
    stop(Pid).

part_error_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "PART"),
    ?assertMatch(<<":localhost 461 cjk101010 PART :need more params!\r\n">>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).

mode_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "MODE cjk101010"),
    <<":localhost 421 cjk101010 +", Mode/binary>> = get_msg(),

    Str = list_to_binary(":cjk101010 MODE cjk101010 :+" ++ trim:trim(binary_to_list(Mode)) ++ "a\r\n"),
    send(Sock, "MODE cjk101010 +a"),
    ?assertMatch(Str, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).

mode_empty_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "MODE cjk101010 +i"),
    receive
        {tcp, _, Data} ->
            gen_tcp:close(Sock),
            stop(Pid),
            throw({error, unexpected_data, Data})
    after
        1000 ->
            ok
    end,

    gen_tcp:close(Sock),
    stop(Pid).

mode_wrong_nick_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "MODE someone_not_there +i"),
    receive
        {tcp, _, Data} ->
            gen_tcp:close(Sock),
            stop(Pid),
            throw({error, unexpected_data, Data})
    after
        1000 ->
            ok
    end,

    gen_tcp:close(Sock),
    stop(Pid).

names_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "JOIN #selfhtml"),
    ?assertMatch(<<":cjk101010!ckruse@", _:32/binary, " JOIN :#selfhtml">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010">>, trim:trim(get_msg())),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml :End of NAMES list">>, trim:trim(get_msg())),

    send(Sock, "NAMES #selfhtml"),
    ?assertMatch(<<":localhost 353 cjk101010 = #selfhtml :cjk101010 \r\n">>, get_msg()),
    ?assertMatch(<<":localhost 366 cjk101010 #selfhtml", _/binary>>, get_msg()),

    send(Sock, "NAMES #wefewfewf"),
    ?assertMatch(<<":localhost 366 cjk101010 #wefewfewf", _/binary>>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).

privmsg_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    {ok, Sock1} = gen_tcp:connect({127,0,0,1}, 6668, [binary, {active, true}, {reuseaddr, true}, {packet, line}, {keepalive, true}]),
    prelude(Sock1, "cjk010101"),

    send(Sock1, "PRIVMSG cjk101010 :just a test"),
    ?assertMatch(<<":cjk010101!ckruse@421AA90E079FA326B6494F812AD13E79 PRIVMSG cjk101010 :just a test\r\n">>, get_msg()),

    gen_tcp:close(Sock),
    gen_tcp:close(Sock1),
    stop(Pid).

privmsg_unknown_test() ->
    {Pid, Sock} = connect(),
    prelude(Sock),

    send(Sock, "PRIVMSG wfewdewfdew :just a test"),
    ?assertMatch(<<":localhost 401 cjk101010 wfewdewfdew :No such nick/channel\r\n">>, get_msg()),

    gen_tcp:close(Sock),
    stop(Pid).


prelude(Sock) ->
    prelude(Sock, "cjk101010").

prelude(Sock, Nick) when is_list(Nick) ->
    prelude(Sock, list_to_binary(Nick));

prelude(Sock, Nick) ->
    NLen = byte_size(Nick),

    ?assertMatch(<<":localhost AUTH NOTICE :*** Looking up your hostname\r\n">>, get_msg()),
    ?assertMatch(<<":localhost AUTH NOTICE :Using hostname localhost\r\n">>, get_msg()),

    send(Sock,["NICK ", Nick]),
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
    ?assertMatch(<<":", Nick:NLen/binary, " MODE", _/binary>>, get_msg()).

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
