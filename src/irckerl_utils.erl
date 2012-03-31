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

-module(irckerl_utils).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-export([to_hex/1, mask_ip/1, mask_host/1, random_str/1, to_unixtimestamp/1, is_int_str/1]).

% @doc This module exposes some helper methods.

% @doc Takes a binary string and returns this hexadecimal representation.
-spec to_hex(binary()) -> string().
to_hex(<<C:1/binary,Rest/binary>>) ->
    lists:flatten(io_lib:format("~2.16.0B",binary_to_list(C)), to_hex(Rest));
to_hex(<<>>) ->
    [].

% @doc Takes a ipv4 or ipv6 IP address and returns it masked.
-spec mask_ip(tuple() | string()) -> string().
mask_ip(Ip) when is_tuple(Ip) ->
    IpStr = case Ip of
        {I1,I2,I3,I4} ->
            integer_to_list(I1) ++ "." ++ integer_to_list(I2) ++ "." ++ integer_to_list(I3) ++ "." ++ integer_to_list(I4);
        {I1,I2,I3,I4,I5,I6,I7,I8} ->
            io_lib:format("~.16B",[I1]) ++ ":"
                ++ io_lib:format("~.16B",[I2]) ++ ":"
                ++ io_lib:format("~.16B",[I3]) ++ ":"
                ++ io_lib:format("~.16B",[I4]) ++ ":"
                ++ io_lib:format("~.16B",[I5]) ++ ":"
                ++ io_lib:format("~.16B",[I6]) ++ ":"
                ++ io_lib:format("~.16B",[I7]) ++ ":"
                ++ io_lib:format("~.16B",[I8])
    end,

    mask_ip(IpStr);

mask_ip(IpStr) ->
    to_hex(crypto:md5(IpStr)).

% @doc Takes a Host string and returns it masked.
-spec mask_host(Host::string()) -> string().
mask_host(Host) ->
    [First|Tail] = re:split(Host, "\\.", [{parts, 2}]),
    MD5Host = to_hex(crypto:md5(First)),

    case Tail of
        [] ->
            MD5Host;
        [Part] ->
            MD5Host ++ "." ++ binary_to_list(Part)
    end.

% @doc Takes the length of the random string and returns
% a random string of this length.
-spec random_str(I::integer()) -> string().
random_str(I) when I > 0 ->
    ValidChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890",
    [lists:nth(random:uniform(length(ValidChars)),ValidChars)] ++ random_str(I-1);
random_str(_) ->
    [].


-spec to_unixtimestamp(erlang:timestamp() | calendar:datetime()) -> integer().
to_unixtimestamp(Timestamp = {_, _, _}) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Timestamp)) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}});
to_unixtimestamp(DateTime = {{_, _, _}, {_, _, _}}) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).


is_int_str([C|Tail]) when (C-48) >= 0 andalso (C-48) =< 9 ->
    is_int_str(Tail);
is_int_str([]) ->
    true;
is_int_str(_) ->
    false.

% eof
