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

-module(irc_parser_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    {ok, RetVal} = irc.parser:parse(<<":my.server.lala NICK :cjk101010">>),
    ?assertEqual({"my.server.lala"} , RetVal#irc_cmd.prefix),
    ?assertEqual("NICK", RetVal#irc_cmd.cmd),
    ?assertEqual([["cjk101010"]], RetVal#irc_cmd.params),

    {ok, RetVal1} = irc.parser:parse(<<"privmsg #lala :cjk101010">>),
    ?assertEqual({}, RetVal1#irc_cmd.prefix),
    ?assertEqual("PRIVMSG", RetVal1#irc_cmd.cmd),
    ?assertEqual([["#lala"], ["cjk101010"]], RetVal1#irc_cmd.params),

    {ok, RetVal2} = irc.parser:parse(<<"join #lala,#lulu pass1,pass2">>),
    ?assertEqual({}, RetVal2#irc_cmd.prefix),
    ?assertEqual("JOIN", RetVal2#irc_cmd.cmd),
    ?assertEqual([["#lala", "#lulu"], ["pass1", "pass2"]], RetVal2#irc_cmd.params),

    {ok, RetVal3} = irc.parser:parse(<<":cjk!lala@my.server NICK :cjk101010">>),
    ?assertEqual({"cjk", "lala", "my.server"} , RetVal3#irc_cmd.prefix),
    ?assertEqual("NICK", RetVal3#irc_cmd.cmd),
    ?assertEqual([["cjk101010"]], RetVal3#irc_cmd.params),

    {ok, RetVal4} = irc.parser:parse(<<":cjk!lala NICK :cjk101010">>),
    ?assertEqual({"cjk", "lala", ""} , RetVal4#irc_cmd.prefix),
    ?assertEqual("NICK", RetVal4#irc_cmd.cmd),
    ?assertEqual([["cjk101010"]], RetVal4#irc_cmd.params).

parse_fail_test() ->
    {error, _} = irc.parser:parse(<<":my&server.lala NICK :cjk101010">>),
    {error, _} = irc.parser:parse(<<"äüö WDWE">>).


% eof
