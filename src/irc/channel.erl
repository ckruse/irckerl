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

-include("irckerl.hrl").

-export([join/3, part/4, privmsg/6, users/2, quit/3, send_messages/3, send_messages/2]).

-import(gen_fsm).
-import(lists).
-import(timer).

-import(irc.utils).

-spec join(#channel_state{}, #channel{}, #user{}) -> {reply, {ok, [string()]}, #channel_state{}}.
join(State, Chan, User = #user{nick = Nick, username = Username, host = Host}) ->
    case check_access(Chan, User) of
        true ->
            Clients = Chan#channel.members ++ [{Nick, User}],

            Names = lists:map(fun({N, #user{pid = CPid}}) ->
                                      gen_fsm:send_event(CPid, {join, Nick ++ "!" ++ Username ++ "@" ++ Host, Chan#channel.name}),
                                      N
                              end, Clients),

            TheChan = Chan#channel{members = Clients},
            {reply, {ok, Names}, State#channel_state{channel = TheChan}};

        _ ->
            {reply, {error, invite_only}, State}
    end.

check_access(Chan, User) ->
    case irc.utils:has_mode($i, Chan) of
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
                    true
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


-spec part(#channel_state{}, #channel{}, #user{}, string()) -> {reply, ok, #channel_state{}}.
part(State, Chan, User, Reason) ->
    LNick   = irc.utils:to_lower(User#user.nick),
    Clients = lists:filter(fun(_ = #user{normalized_nick = N}) -> N =/= LNick end, Chan#channel.members),

    send_messages(Chan#channel.members, {msg, [":", irc.utils:full_nick(User), " PART ", Chan#channel.name, " :", Reason, "\r\n"]}),

    {reply, ok, State#channel_state{channel=Chan#channel{members=Clients}}}.

-spec privmsg(#channel_state{}, #channel{}, string(), string(), string(), string()) -> {reply, ok, #channel_state{}}.
privmsg(State, Chan, Nick, From, To, Message) ->
    send_messages(Chan#channel.members, Nick, {privmsg, From, To, Message}),
    {reply, ok, State}.

users(State, Chan) ->
    {reply, {ok, Chan#channel.members}, State}.

-spec quit(#channel_state{}, #user{}, string()) -> {noreply, #channel_state{}}.
quit(State, User, Reason) ->
    Members = lists:filter(fun(U) -> U#user.normalized_nick =/= User#user.normalized_nick end, State#channel_state.channel#channel.members),
    send_messages(Members, {msg, [":", irc.utils:full_nick(User), " QUIT :", Reason, "\015\012"]}),

    {noreply, State#channel_state{channel = #channel{members = Members}}}.


-spec send_messages([#user{}], string(), any()) -> ok.
send_messages([], _, _) ->
    ok;
send_messages([{_, User}|Tail], Nick, Data) ->
    case Nick == User#user.nick of
        true ->
            send_messages(Tail, Nick, Data);
        _ ->
            gen_fsm:send_event(User#user.pid, Data),
            send_messages(Tail, Nick, Data)
    end.

send_messages([], _) ->
    ok;
send_messages([{_,User}|Tail], Data) ->
    gen_fsm:send_event(User#user.pid, Data),
    send_messages(Tail, Data).

% eof
