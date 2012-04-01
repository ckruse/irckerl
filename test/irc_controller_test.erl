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

-module(irc_controller_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

choose_nick_test() ->
    State = #controller_state{reserved_nicks = dict:new(), clients = []},
    {reply, ok, NState} = irc_controller:choose_nick(State, "cjk101010", "cjk101010", #user{}),
    R = NState#controller_state.reserved_nicks,
    ?assertMatch(
       {ok, _},
       dict:find("cjk101010", R)
      ),

    ?assertMatch(
       {reply, nick_registered_already, _},
       irc_controller:choose_nick(NState, "cjk101010", "cjk101010", #user{})
      ).

get_user_test() ->
    State = #controller_state{reserved_nicks = dict:append("cjk101010", #user{}, dict:new())},
    ?assertMatch(
       {reply, {ok, #user{}}, _},
       irc_controller:get_user(State, "cjk101010")
      ),

    ?assertMatch(
       {reply, {error, _}, _},
       irc_controller:get_user(State, "wefewfwfe")
      ).

get_channel_test() ->
    State = #controller_state{channels = dict:append("#selfhtml", self(), dict:new())},
    ?assertMatch(
       {reply, {ok, _}, _},
       irc_controller:get_channel(State, "#selfhtml")
      ),
    ?assertMatch(
       {reply, {error, _}, _},
       irc_controller:get_channel(State, "#wfwf")
      ).

delete_nick_test() ->
    State = #controller_state{reserved_nicks = dict:append("cjk101010", #user{}, dict:new())},
    {noreply, NState} = irc_controller:delete_nick(State, "cjk101010"),
    {noreply, _} = irc_controller:delete_nick(NState, "cjk101010"),
    ?assertMatch(
       error,
       dict:find("cjk101010", NState#controller_state.reserved_nicks)
      ).

% eof
