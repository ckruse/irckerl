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

-module(irc_channel).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-include("irckerl.hrl").

-export([join/4, part/4, privmsg/6, users/2, quit/3]).

-spec join(#channel_state{}, #channel{}, #user{}, string()) -> {reply, {ok, [string()]}, #channel_state{}}.
join(State, Chan, User = #user{nick = Nick, username = Username, masked = Host}, Pass) ->
    case irc_channel_helpers:check_access(Chan, User, Pass) of
        true ->
            Usr = case length(Chan#channel.members) of
                      0 ->
                          #chan_user{user = User, level = ?U_LEVEL_OP, mode = "o"};
                      _ ->
                          #chan_user{user = User, level = 0}
                  end,

            Clients = Chan#channel.members ++ [Usr],
            Names = lists:map(fun(#chan_user{user = #user{pid = CPid, nick = N}}) ->
                                      gen_fsm:send_event(CPid, {join, Nick ++ "!" ++ Username ++ "@" ++ Host, Chan#channel.name}),
                                      N
                              end, Chan#channel.members),

            TheChan = Chan#channel{members = Clients},
            {reply, {ok, Names ++ [Nick]}, State#channel_state{channel = TheChan}};

        _ ->
            {reply, {error, invite_only}, State}
    end.


-spec part(#channel_state{}, #channel{}, #user{}, string()) -> {reply, ok, #channel_state{}}.
part(State, Chan, User, Reason) ->
    LNick   = irc_utils:to_lower(User#user.nick),
    Clients = lists:filter(fun(#chan_user{user = #user{normalized_nick = N}}) -> N =/= LNick end, Chan#channel.members),

    irc_channel_helpers:send_messages(Chan#channel.members, {msg, [":", irc_utils:full_nick(User), " PART ", Chan#channel.name, " :", Reason, "\r\n"]}),

    {reply, ok, State#channel_state{channel=Chan#channel{members=Clients}}}.


-spec privmsg(#channel_state{}, #channel{}, string(), string(), string(), string()) -> {reply, ok, #channel_state{}}.
privmsg(State, Chan, Nick, From, To, Message) ->
    irc_channel_helpers:send_messages(Chan#channel.members, Nick, {privmsg, From, To, Message}),
    {reply, ok, State}.

users(State, Chan) ->
    {reply, {ok, [X#chan_user.user || X <- Chan#channel.members]}, State}.

-spec quit(#channel_state{}, #user{}, string()) -> {noreply, #channel_state{}}.
quit(State, User, Reason) ->
    Members = lists:filter(fun(#chan_user{user = U}) -> U#user.normalized_nick =/= User#user.normalized_nick end, State#channel_state.channel#channel.members),
    irc_channel_helpers:send_messages(Members, {msg, [":", irc_utils:full_nick(User), " QUIT :", Reason, "\015\012"]}),

    {noreply, State#channel_state{channel = #channel{members = Members}}}.



% eof
