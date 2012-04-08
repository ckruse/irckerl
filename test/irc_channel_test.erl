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

-module(irc_channel_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

check_access_test() ->
    Chan = #channel{mode = "i", invite_list = [], members = []},
    Usr = #user{nick = "cjk101010"},
    ?assert(irc_channel:check_access(Chan, Usr, "") == false),
    ?assert(irc_channel:check_access(Chan#channel{invite_list = [{erlang:now(), #user{nick = "lwwefwef"}}, {erlang:now(), Usr}]}, Usr, "") == true),
    ?assert(irc_channel:check_access(Chan#channel{invite_list = [{{0, 0, 0}, Usr}]}, Usr, "") == false),
    ?assert(irc_channel:check_access(Chan#channel{mode = "k", password = "gjm270z"}, Usr, "") == false),
    ?assert(irc_channel:check_access(Chan#channel{mode = "k", password = "gjm270z"}, Usr, "gjm270z") == true),
    ?assert(irc_channel:check_access(Chan#channel{members = [1, 2, 3, 4, 5, 6], mode = "l", limit = 1}, Usr, "") == false).

users_test() ->
    Members = ["a", "b", "c"],
    ?assertMatch(
       {reply, {ok, Members}, {}},
       irc_channel:users({}, #channel{members = Members})
      ).


% eof
