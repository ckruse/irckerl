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

to_hex_test() ->
    ?assert(utils:to_hex(<<"abc">>) == "616263"),
    ?assert(utils:to_hex(<<0, "abc">>) == "00616263"),
    ?assert(utils:to_hex(<<>>) == "").

mask_ip_test() ->
    crypto:start(),

    MD5Localhost = utils:to_hex(crypto:md5("127.0.0.1")),
    MD5IPv6Localhost = utils:to_hex(crypto:md5("0:0:0:0:0:0:0:1")),

    ?assertEqual(MD5Localhost, utils:mask_ip("127.0.0.1")),
    ?assertEqual(MD5Localhost, utils:mask_ip({127, 0, 0, 1})),
    ?assertEqual(MD5IPv6Localhost, utils:mask_ip("0:0:0:0:0:0:0:1")),
    ?assertEqual(MD5IPv6Localhost, utils:mask_ip({0, 0, 0, 0, 0, 0, 0, 1})),

    crypto:stop().


mask_host_test() ->
    crypto:start(),

    MD5Host = utils:to_hex(crypto:md5("localhost")),
    ?assertEqual(utils:mask_host("localhost"), MD5Host),

    MD5Motte = utils:to_hex(crypto:md5("motte")),
    ?assertEqual(utils:mask_host("motte.local.defunced.de"), MD5Motte ++ ".local.defunced.de"),

    crypto:stop().

random_str_test() ->
    ?assertEqual(utils:random_str(0), []),
    ?assertEqual(length(utils:random_str(8)), 8).

is_int_str_test() ->
    ?assert(utils:is_int_str("123")),
    ?assert(utils:is_int_str("034234")),
    ?assertNot(utils:is_int_str("0.34")),
    ?assertNot(utils:is_int_str("0,34")),
    ?assertNot(utils:is_int_str("abcd")),
    ?assertNot(utils:is_int_str("32423addfwe")).

to_unixtimestamp_test() ->
    ?assertEqual(utils:to_unixtimestamp({{1970, 1, 1}, {0, 0, 0}}), 0),
    ?assertEqual(utils:to_unixtimestamp({{1970, 1, 1}, {1, 0, 0}}), 3600),

    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    ?assertEqual(utils:to_unixtimestamp({MegaSecs, Secs, MicroSecs}), (MegaSecs * 1000000 + Secs)).

% eof