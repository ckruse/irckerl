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

-module(irckerl_controller_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test() ->
    {ok, _Pid} = irckerl_controller:start_link([]),
    irckerl_controller:stop().

created_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       {created, {{_, _, _}, {_, _, _}}},
       gen_server:call(Pid, created)
      ),
    stop(Pid).

count_users_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       {visible, _, invisible, _},
       gen_server:call(Pid, count_users)
      ),
    stop(Pid).

count_servers_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       {servers, _},
       gen_server:call(Pid, count_servers)
      ),
    stop(Pid).


register_client_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {register_client, self()})
      ),
    stop(Pid).

register_client_fail_test() ->
    Pid = start([{limits, [{maxusers, 0}]}]),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       {error, max_connect},
       gen_server:call(Pid, {register_client, self()})
      ),
    stop(Pid).

choose_nick_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {register_client, self()})
      ),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {choose_nick, "cjk101010", "cjk101010", #user{pid = self()}})
      ),
    stop(Pid).

delete_nick_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {register_client, self()})
      ),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {choose_nick, "cjk101010", "cjk101010", #user{pid = self()}})
      ),
    ?assertMatch(
       ok,
       gen_server:cast(Pid, {delete_nick, "cjk101010"})
      ),
    stop(Pid).

join_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       {ok, _, ["cjk101010"]},
       gen_server:call(Pid, {join, "#erlang", #user{nick = "cjk101010", username = "ckruse", masked = "localhost", pid = self()}, ""})
      ),
    stop(Pid).

get_user_test() ->
    Pid = start(),
    ?assert(is_pid(Pid)),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {register_client, self()})
      ),
    ?assertMatch(
       ok,
       gen_server:call(Pid, {choose_nick, "cjk101010", "cjk101010", #user{pid = self()}})
      ),
    ?assertMatch(
       {ok, #user{}},
       gen_server:call(Pid, {get_user, "cjk101010"})
      ),
    stop(Pid).



start() ->
    {ok, Pid} = irckerl_controller:start_link([]),
    Pid.
start(Settings) ->
    {ok, Pid} = irckerl_controller:start_link(Settings),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop).


% eof
