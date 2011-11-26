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

-module(utils_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include_lib("eunit/include/eunit.hrl").

random_str_test() ->
    ?assert(utils:random_str(0) == []),
    ?assert(length(utils:random_str(8)) == 8).

is_int_str_test() ->
    ?assert(utils:is_int_str("123")),
    ?assert(utils:is_int_str("034234")),
    ?assertNot(utils:is_int_str("0.34")),
    ?assertNot(utils:is_int_str("0,34")),
    ?assertNot(utils:is_int_str("abcd")),
    ?assertNot(utils:is_int_str("32423addfwe")).

to_unixtimestamp_test() ->
    ?assert(utils:to_unixtimestamp({{1970, 1, 1}, {0, 0, 0}}) == 0),
    ?assert(utils:to_unixtimestamp({{1970, 1, 1}, {1, 0, 0}}) == 3600),
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    ?assert(utils:to_unixtimestamp({MegaSecs, Secs, MicroSecs}) == (MegaSecs * 1000000 + Secs)).

% eof