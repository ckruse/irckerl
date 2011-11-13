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

-module(irc.channel).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-include("../irckerl.hrl").

-export([join/3, part/4, privmsg/6, users/2, send_messages/3, send_messages/2]).

-import(gen_fsm).
-import(lists).

-import(irckerl_parser).

-spec join(#channel_state{}, #channel{}, #user{}) -> {reply, {ok, [string()]}, #channel_state{}}.
join(State, Chan, User = #user{nick = Nick, username = Username, host = Host}) ->
    Clients = Chan#channel.members ++ [User],
    Names = lists:map(fun(_ = #user{nick = N, pid = CPid}) ->
                              gen_fsm:send_event(CPid, {join, Nick ++ "!" ++ Username ++ "@" ++ Host, Chan#channel.name}),
                              N
                      end, Clients),
    {reply, {ok, Names}, State#channel_state{channel=Chan#channel{members=Clients}}}.

-spec part(#channel_state{}, #channel{}, #user{}, string()) -> {reply, ok, #channel_state{}}.
part(State, Chan, User, Reason) ->
    LNick   = irckerl_parser:to_lower(User#user.nick),
    Clients = lists:filter(fun(_ = #user{normalized_nick = N}) -> N =/= LNick end, Chan#channel.members),
    send_messages(Chan#channel.members, {msg, [":", irckerl_parser:full_nick(User), " PART ", Chan#channel.name, " :", Reason, "\r\n"]}),
    {reply, ok, State#channel_state{channel=Chan#channel{members=Clients}}}.

-spec privmsg(#channel_state{}, #channel{}, string(), string(), string(), string()) -> {reply, ok, #channel_state{}}.
privmsg(State, Chan, Nick, From, To, Message) ->
    send_messages(Chan#channel.members, Nick, {privmsg, From, To, Message}),
    {reply, ok, State}.

users(State, Chan) ->
    {reply, {ok, Chan#channel.members}, State}.


-spec send_messages([#user{}], string(), any()) -> ok.
send_messages([], _, _) ->
    ok;
send_messages([User|Tail], Nick, Data) ->
    case Nick == User#user.nick of
        true ->
            send_messages(Tail, Nick, Data);
        _ ->
            gen_fsm:send_event(User#user.pid, Data),
            send_messages(Tail, Nick, Data)
    end.

send_messages([], _) ->
    ok;
send_messages([User|Tail], Data) ->
    gen_fsm:send_event(User#user.pid, Data),
    send_messages(Tail, Data).

% eof