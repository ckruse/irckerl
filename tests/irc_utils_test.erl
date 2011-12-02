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

-module(irc_utils_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

to_lower_test() ->
    ?assertEqual("abc", irc.utils:to_lower("abc")),
    ?assertEqual("{abc}", irc.utils:to_lower("[abc]")),
    ?assertEqual("|abc", irc.utils:to_lower("\\abc")).

full_nick_test() ->
    Usr = #user{nick = "cjk101010", username = "ckruse", masked = "abcdef"},
    ?assertEqual("cjk101010!ckruse@abcdef", irc.utils:full_nick(Usr)).

valid_nick_test() ->
    ?assertEqual(valid, irc.utils:valid_nick("cjk101010", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("cjk1010101010101010101010101010101010101010", [])),
    ?assertEqual(valid, irc.utils:valid_nick("cjk`{}[]-\\_", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("cjk,", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("cjk;", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("cjk.", [])),
    ?assertEqual(invalid, irc.utils:valid_nick("cjk.", [])).

valid_channel_test() ->
    ?assertEqual(valid, irc.utils:valid_channel("#selfhtml")),
    ?assertEqual(valid, irc.utils:valid_channel("&selfhtml")),
    ?assertEqual(invalid, irc.utils:valid_channel("#self html")),
    ?assertEqual(invalid, irc.utils:valid_channel("#self\7html")),
    ?assertEqual(invalid, irc.utils:valid_channel("#self,html")),
    ?assertEqual(invalid, irc.utils:valid_channel("#")),
    ?assertEqual(invalid, irc.utils:valid_channel("")).

% eof
