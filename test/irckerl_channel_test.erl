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

-module(irckerl_channel_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", "m"),
    ?assert(is_pid(Pid)),
    ?assert(gen_server:call(Pid, stop) == ok).

topic_fail_test() ->
    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", ""),
    ?assertMatch(
       {ok, none},
       gen_server:call(Pid, topic)
      ),

    ?assertMatch(
       {error, not_on_channel},
       gen_server:call(Pid, {topic, "Lulu", #user{nick = "cjk101010", masked = "localhost", username = "ckruse"}})
      ),

    gen_server:call(Pid, stop).

users_test() ->
    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", ""),
    ?assertMatch(
       {ok, []},
       gen_server:call(Pid, get_users)
      ),
    gen_server:call(Pid, stop).

unknown_test() ->
    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", ""),
    ?assertMatch(
       ok,
       gen_server:call(Pid, weifife)
      ),
    gen_server:call(Pid, stop).

join_part_test() ->
    Usr = #user{
      nick = "cjk101010",
      normalized_nick = "cjk101010",
      masked = "localhost",
      username = "ckruse",
      pid = self()
     },

    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", ""),
    ?assertMatch(
       {ok, [#chan_user{user = #user{nick = "cjk101010"}}]},
       gen_server:call(Pid, {join, Usr, ""})
      ),

    ?assertMatch(
       ok,
       gen_server:call(Pid, {part, Usr, "part"})
      ),

    receive
        {'$gen_event', {msg, [":","cjk101010!ckruse@localhost", " PART ", "#selfhtml", " :", "part", "\r\n"]}} ->
            ok
    after
        5000 ->
            throw({error, receive_timeout})
    end,

    gen_server:call(Pid, stop).


topic_test() ->
    {ok, Pid} = irckerl_channel:start_link([], "#selfhtml", ""),
    ?assertMatch(
       {ok, none},
       gen_server:call(Pid, topic)
      ),

    Usr = #user{
      nick = "cjk101010",
      normalized_nick = "cjk101010",
      masked = "localhost",
      username = "ckruse",
      pid = self()
     },

    ?assertMatch(
       {ok, [#chan_user{user = #user{nick = "cjk101010"}}]},
       gen_server:call(Pid, {join, Usr, ""})
      ),

    ?assertMatch(
       ok,
       gen_server:call(Pid, {topic, "Lulu", Usr})
      ),

    gen_server:call(Pid, stop).



% eof
