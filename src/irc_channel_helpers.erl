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

-module(irc_channel_helpers).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-export([check_access/3]).

check_access(Chan, User, Pass) ->
    case irc_utils:has_mode($i, Chan) of
        true ->
            case is_in_invite_list(Chan#channel.invite_list, User) of
                true ->
                    true;
                _ ->
                    false
            end;

        _ ->
            case (Chan#channel.limit > 0) and (length(Chan#channel.members) >= Chan#channel.limit) of
                true ->
                    false;
                _ ->
                    case irc_utils:has_mode($k, Chan) of
                        true ->
                            case Chan#channel.password == Pass of
                                true ->
                                    true;
                                _ ->
                                    false
                            end;

                        _ ->
                            true
                    end
            end
    end.


is_in_invite_list([{Ts, Usr} | Tail], User) ->
    case Usr#user.nick == User#user.nick of
        true ->
            case timer:now_diff(erlang:now(), Ts) > 10 * 60 * 1000 of
                true ->
                    false;
                _ ->
                    true
            end;
        false ->
            is_in_invite_list(Tail, User)
    end;
is_in_invite_list([], _) ->
    false.

% eof
