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

% TODO: implement real tests!

-module(irc_utils_test).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-include_lib("eunit/include/eunit.hrl").

to_lower_test() ->
    ?assertEqual("cjk101010", irc_utils:to_lower("CJK101010")),
    ?assertEqual("cjk{}|", irc_utils:to_lower("cjk[]\\")).


full_nick_test() ->
    ?assertEqual("cjk101010!ckruse@localhost", irc_utils:full_nick(#user{nick = "cjk101010", username = "ckruse", masked = "localhost"})).

valid_nick_test() ->
    ?assertEqual(valid, irc_utils:valid_nick("cjk101010", [])),
    ?assertEqual(valid, irc_utils:valid_nick("cjk101010[]", [])),
    ?assertEqual(valid, irc_utils:valid_nick("cjk101010_", [])),
    ?assertEqual(invalid, irc_utils:valid_nick("cjk101010!!", [])),
    ?assertEqual(invalid, irc_utils:valid_nick("cjk1010102398230ru2fr90mf9432nf32md932fn32f932nf32nf32932n923nf92n329fn32f92u3fn329fnu239f23ufn293nf32fnu32nufd329f932nf", [])).

valid_channel_test() ->
    ?assertEqual(valid, irc_utils:valid_channel("#selfhtml")),
    ?assertEqual(valid, irc_utils:valid_channel("&selfhtml")),
    ?assertEqual(invalid, irc_utils:valid_channel("#self html")),
    ?assertEqual(invalid, irc_utils:valid_channel("#self,html")),
    ?assertEqual(invalid, irc_utils:valid_channel("#self" ++ [7] ++ "html")),
    ?assertEqual(invalid, irc_utils:valid_channel("#selfcjk1010102398230ru2fr90mf9432nf32md932fn32f932nf32nf32932n923nf92n329fn32f92u3fn329fnu239f23ufn293nf32fnu32nufd329f932nfweifh8ewf8ewfhewfrw2fw2efdiewhfdiewhfiewfhewifhewifhewifewfiewewfiwefewfselfcjk1010102398230ru2fr90mf9432nf32md932fn32f932nf32nf32932n923nf92n329fn32f92u3fn329fnu239f23ufn293nf32fnu32nufd329f932nfweifh8ewf8ewfhewfrw2fw2efdiewhfdiewhfiewfhewifhewifhewifewfiewewfiwefewf")),
    ?assertEqual(invalid, irc_utils:valid_channel("&selfcjk1010102398230ru2fr90mf9432nf32md932fn32f932nf32nf32932n923nf92n329fn32f92u3fn329fnu239f23ufn293nf32fnu32nufd329f932nfweifh8ewf8ewfhewfrw2fw2efdiewhfdiewhfiewfhewifhewifhewifewfiewewfiwefewfselfcjk1010102398230ru2fr90mf9432nf32md932fn32f932nf32nf32932n923nf92n329fn32f92u3fn329fnu239f23ufn293nf32fnu32nufd329f932nfweifh8ewf8ewfhewfrw2fw2efdiewhfdiewhfiewfhewifhewifhewifewfiewewfiwefewf")),
    ?assertEqual(invalid, irc_utils:valid_channel("wwefwef")).

has_mode_test() ->
    ?assert(irc_utils:has_mode($m, #channel{mode = "miu"})),
    ?assertEqual(false, irc_utils:has_mode($m, #channel{mode = "iu"})),
    ?assert(irc_utils:has_mode($m, #user{mode = "miu"})),
    ?assertEqual(false, irc_utils:has_mode($m, #channel{mode = "iu"})).

valid_user_test() ->
    ?assert(irc_utils:valid_user(<<"ckruse">>)  == valid),
    ?assert(irc_utils:valid_user(<<"ckruse@ewewf">>) == invalid),
    ?assert(irc_utils:valid_user(<<"ckruse\nabc">>) == invalid),
    ?assert(irc_utils:valid_user(<<"ckruse\rabc">>) == invalid),
    ?assert(irc_utils:valid_user(<<"ckruse wfwqf">>) == invalid),
    ?assert(irc_utils:valid_user(<<"ckruse\0fwe">>) == invalid),
    ?assert(irc_utils:valid_user(<<"">>) == invalid).

may_test() ->
    ?assert(irc_utils:may(topic, #channel{mode = ""}, #chan_user{mode = ""})),
    ?assert(irc_utils:may(topic, #channel{mode = "t"}, #chan_user{mode = ""}) == false),
    ?assert(irc_utils:may(topic, #channel{mode = "t"}, #chan_user{mode = "o"})),


    ?assert(irc_utils:may(privmsg, #channel{mode = ""}, #chan_user{mode = ""})),
    ?assert(irc_utils:may(privmsg, #channel{mode = "m"}, #chan_user{mode = ""}) == false),
    ?assert(irc_utils:may(privmsg, #channel{mode = "m"}, #chan_user{mode = "o"})),
    ?assert(irc_utils:may(privmsg, #channel{mode = "m"}, #chan_user{mode = "v"})).


% eof
