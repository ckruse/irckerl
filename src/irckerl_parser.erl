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

-module(irckerl_parser).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-export([parse/1, normalize_nick/1]).


normalize_nick("[" ++ Rest)  -> "{" ++ normalize_nick(Rest);
normalize_nick("]" ++ Rest)  -> "}" ++ normalize_nick(Rest);
normalize_nick("\\" ++ Rest) -> "|" ++ normalize_nick(Rest);
normalize_nick([Head|Tail])  -> string:to_lower([Head]) ++ normalize_nick(Tail);
normalize_nick([])           -> [].

parse_prefix(<<":",Prefix/binary>>) ->
    parse_prefix(Prefix);
parse_prefix(<<Prefix/binary>>) ->
    io:format("prefix: ~p~n",[Prefix]),
    case re:run(Prefix,"^([a-z0-9.-]+)$",[{capture,none}]) of
        match ->
            {ok, [binary_to_list(Prefix)]};

        nomatch ->
            case re:run(Prefix,"^([a-zA-Z][a-zA-Z0-9\\[\\]\\\\^{}`-]+)(?:!([^\s@]+)(?:@([a-z0-9.-]+))?)?$",[{capture,all_but_first}]) of
                {match,[{StartN, EndN}]} ->
                    {ok, [string:substr(binary_to_list(Prefix),StartN+1,EndN),"",""]};

                {match,[{StartN, EndN},{StartU,EndU}]} ->
                    LPrefix = binary_to_list(Prefix),
                    {ok,[string:substr(LPrefix,StartN+1,EndN),string:substr(LPrefix,StartU+1,EndU),""]};

                {match,[{StartN, EndN},{StartU,EndU},{StartH,EndH}]} ->
                    LPrefix = binary_to_list(Prefix),
                    {ok,[string:substr(LPrefix,StartN+1,EndN),string:substr(LPrefix,StartU+1,EndU),string:substr(LPrefix,StartH+1,EndH)]};

                nomatch ->
                    {error, "Prefix parse: not matched to prefix regex"};
                _ ->
                    {error, "Prefix parse: not matched to prefix regex"}
            end;

        _ ->
            {error, "Prefix parse: not matched to prefix regex"}
    end.


parse(<<":",Message/binary>>) -> % a message with a prefix
    [PrefixS,LastS] = re:split(Message,"\s+",[{parts,2}]),
    case parse_prefix(PrefixS) of
        {ok, Prefix} ->
            case parse(LastS) of
                {ok, Cmd, Params} ->
                    {ok, Prefix, Cmd, Params};
                {error, Reason} ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end;

parse(<<Message/binary>>) -> % a message w/o prefix
    case re:run(Message,"^([a-zA-Z]+|\\d\\d\\d)",[{capture,all_but_first}]) of
        {match,[{Start,Len}]} ->
            LMessage = binary_to_list(Message),
            Cmd = string:to_lower(string:substr(LMessage,Start+1,Len)),
            Params = parse_params(trim:trim(string:substr(LMessage,Start+Len+1))),
            {ok, {}, string:to_upper(Cmd), Params};

        nomatch -> {error, "not an expected command token"}
    end.


parse_params(":" ++ ParamStr) ->
    [ParamStr];
parse_params(ParamStr) ->
    case re:split(ParamStr,"\s+",[{parts,2},{return,list}]) of
        [First,Last] ->
            [First|parse_params(Last)];
        [First] ->
            [First]
    end.



% eof
