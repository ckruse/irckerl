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

parse_prefix_test() ->
    ?assertMatch(
       {ok, #irc_cmd{prefix = {"gagarin.epd-me.net"}, cmd = "NOTICE", params = [["AUTH"], ["*** Looking up your hostname..."]]}},
       irc_parser:parse(<<":gagarin.epd-me.net NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertMatch(
       {ok, #irc_cmd{prefix = {}, cmd = "NOTICE", params = [["AUTH"], ["*** Looking up your hostname..."]]}},
       irc_parser:parse(<<"NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertMatch(
       {ok, #irc_cmd{prefix = {"cjk101010", "ckruse", "localhost"}, cmd = "NOTICE", params = [["AUTH"], ["*** Looking up your hostname..."]]}},
       irc_parser:parse(<<":cjk101010!ckruse@localhost NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertMatch(
       {ok, #irc_cmd{prefix = {"cjk101010", "ckruse", ""}, cmd = "NOTICE", params = [["AUTH"], ["*** Looking up your hostname..."]]}},
       irc_parser:parse(<<":cjk101010!ckruse NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertMatch(
       {ok, #irc_cmd{prefix = {"cjk101010[]", "", ""}, cmd = "NOTICE", params = [["AUTH"], ["*** Looking up your hostname..."]]}},
       irc_parser:parse(<<":cjk101010[] NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertMatch(
       {error, _},
       irc_parser:parse(<<":cjk101010__ NOTICE AUTH :*** Looking up your hostname...">>)
      ),
    ?assertEqual(
       {ok, {"cjk101010"}},
       irc_parser:parse_prefix(<<":cjk101010">>)
      ).

parse_test() ->
    ?assertMatch(
       {error, _},
       irc_parser:parse(<<":lala __">>)
      ),
    ?assertMatch(
       {ok, #irc_cmd{prefix = {}, cmd = "JOIN", params = [["#lala","#lulu"]]}},
       irc_parser:parse(<<"JOIN #lala,#lulu">>)
      ).



% eof
