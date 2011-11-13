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

-module(utils).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-include("irckerl.hrl").

-export([to_hex/1, mask_ip/1, mask_host/1, random_str/1, valid_nick/2, valid_channel/1, to_unixtimestamp/1]).

% @doc This module exposes some helper methods.

% @doc Takes a binary string and returns this hexadecimal representation.
-spec to_hex(Str::binary()) -> string().
to_hex(<<C:1/binary,Rest/binary>>) ->
    lists:flatten(io_lib:format("~2.16.0B",binary_to_list(C)), to_hex(Rest));
to_hex(<<>>) ->
    [].

% @doc Takes a ipv4 or ipv6 IP address and returns it masked.
-spec mask_ip(Ip::tuple() | string()) -> string().
mask_ip(Ip) when is_tuple(Ip) ->
    IpStr = case Ip of
                {I1,I2,I3,I4} ->
                    integer_to_list(I1) ++ "." ++ integer_to_list(I2) ++ "." ++ integer_to_list(I3) ++ "." ++ integer_to_list(I4);
                {I1,I2,I3,I4,I5,I6,I7,I8} ->
                    to_hex(I1) ++ ":" ++ to_hex(I2) ++ ":" ++ to_hex(I3) ++ ":" ++ to_hex(I4) ++ ":" ++ to_hex(I5) ++ ":" ++ to_hex(I6) ++ ":" ++ to_hex(I7) ++ ":" ++ to_hex(I8)
            end,
    mask_ip(IpStr);

mask_ip(IpStr) ->
    MD5 = crypto:md5(IpStr),
    to_hex(MD5).

% @doc Takes a Host string and returns it masked.
-spec mask_host(Host::string()) -> string().
mask_host(Host) ->
    [First|Tail] = re:split("\.",Host,[{parts,2}]),
    MD5Host = crypto:md5(First),
    to_hex(MD5Host) ++ ["."|Tail].

% @doc Takes the length of the random string and returns
% a random string of this length.
-spec random_str(I::integer()) -> string().
random_str(I) when I >= 0 ->
    ValidChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890",
    [lists:nth(random:uniform(length(ValidChars)),ValidChars)] ++ random_str(I-1);
random_str(_) ->
    [].

% @doc Validates a nick, returns either the atom valid or invalid.
-spec valid_nick(Nick::string(), Settings::proplist()) -> valid | invalid.
valid_nick(Nick,Settings) ->
    Lim = proplists:get_value(limits,Settings,[]),
    case length(Nick) > proplists:get_value(nicklen,Lim,30) of
        true ->
            invalid;

        _ ->
            case re:run(Nick,"^[a-zA-Z][a-zA-Z0-9\\[\\]\\\\^{}`-]+$",[{capture,none}]) of %"
                match  -> valid;
                _ -> invalid
            end
    end.

-spec valid_channel(Chan::string() | binary()) -> valid | invalid.
valid_channel(Str) when is_list(Str) ->
    valid_channel(list_to_binary(Str));

valid_channel(<<"#", Token/binary>>) ->
    case (byte_size(Token) < 200) and (byte_size(Token) > 0) of
        true ->
            valid_channel_name(Token);
        false ->
            invalid
    end;

valid_channel(<<"&", Token/binary>>) ->
    case (byte_size(Token) < 200) and (byte_size(Token) > 0) of
        true ->
            valid_channel_name(Token);
        false ->
            invalid
    end;

valid_channel(_) ->
    invalid.

-spec valid_channel_name(Chan::binary()) -> valid | invalid.
valid_channel_name(<<" ", _/binary>>) ->
    invalid;
valid_channel_name(<<7, _/binary>>) -> % control g, not allowed
    invalid;
valid_channel_name(<<",", _/binary>>) ->
    invalid;
valid_channel_name(<<_:1/binary, Rest/binary>>) ->
    valid_channel_name(Rest);
valid_channel_name(<<>>) ->
    valid.

-spec to_unixtimestamp(erlang:timestamp() | calendar:datetime()) -> integer().
to_unixtimestamp(Timestamp = {_, _, _}) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Timestamp)) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}});
to_unixtimestamp(DateTime = {{_, _, _}, {_, _, _}}) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).


% eof
