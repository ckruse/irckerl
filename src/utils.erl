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


-export([to_hex/1, mask_ip/1, mask_host/1, random_str/1]).


to_hex(<<C:1/binary,Rest/binary>>) ->
    lists:flatten(io_lib:format("~2.16.0B",binary_to_list(C)), to_hex(Rest));
to_hex(<<>>) ->
    [].


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


mask_host(Host) ->
    [First|Tail] = re:split("\.",Host,[{parts,2}]),
    MD5Host = crypto:md5(First),
    to_hex(MD5Host) ++ ["."|Tail].


random_str(I) when I >= 0 ->
    ValidChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890",
    [lists:nth(random:uniform(length(ValidChars)),ValidChars)] ++ random_str(I-1);
random_str(_) ->
    [].

% eof
