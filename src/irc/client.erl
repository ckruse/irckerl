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

-module(irc.client).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-export([names/2, mode/3, mode/2, join/2, privmsg/3, who/2, ping/3, pong/3]).

-include("../irckerl.hrl").
-include("../umodes.hrl").
-include("../cmodes.hrl").

-import(proplists).
-import(io).
-import(re).
-import(lists).
-import(gen_server).
-import(timer).
-import(gen_fsm).

-import(trim).
-import(irckerl_parser).
-import(utils).

-import(irc.client.helpers).
-import(irc.client.ping_pong).

join(State, Chan) ->
    case Chan of
        "0" ->
            ok;
        _ ->
            Channels = re:split(Chan, ","),
            ?DEBUG("state is: ~p~n",[State]),
            lists:map(fun(TheChanB) ->
                              TheChan = binary_to_list(TheChanB),
                              case gen_server:call(irckerl, {join, TheChan, State#client_state.user}) of
                                  {ok, Names} ->
                                      Str = trim:trim(lists:map(fun(N) -> io:format("n: ~p~n",[N]), N ++ " " end, Names)),
                                      helpers:send(State#client_state.socket, [":", irckerl_parser:full_nick(State#client_state.user), " JOIN :", Chan, "\r\n"]),
                                      helpers:send(State, "353", ["= ", TheChan, " :", Str]),
                                      helpers:send(State, "366", [TheChan, " :End of NAMES list"]);
                                  {error, Error} ->
                                      helpers:send(State, "437", ["#", TheChan, ":Nick/channel is temporarily unavailable ", Error]); % TODO: real error messages
                                  {error, unexpected_error, Error} ->
                                      helpers:send(State, "437", ["#", TheChan, ":Nick/channel is temporarily unavailable ", Error]) % TODO: real error messages
                              end
                      end, Channels)
    end,
    {next_state, ready, ping_pong:reset_timer(State)}.

mode(State, Nick) ->
    case irckerl_parser:to_lower(Nick) == State#client_state.user#user.normalized_nick of
        true ->
            helpers:send(State, "421", [State#client_state.user#user.nick, " +", State#client_state.user#user.mode]),
            {next_state, ready, ping_pong:reset_timer(State)};
        _ ->
            {next_state, ready, ping_pong:reset_timer(State)}
    end.

mode(State, Nick, "+" ++ Mode) -> % TODO: there may be a -<modes>
    case irckerl_parser:to_lower(Nick) == State#client_state.user#user.normalized_nick of
        true ->
            NMode = lists:filter(
                      fun(X) ->
                              lists:all(fun(Y) when Y =/= X, X =/= 'o', X =/= 'O' -> true;
                                           (_) -> false
                                        end, State#client_state.user#user.mode)
                      end, Mode),

            case lists:member('a', NMode) of
                true ->
                    NState = State#client_state{away="I'm away"};
                _ ->
                    NState = State
            end,

            case NMode of
                [] ->
                    {next_state, ready, ping_pong:reset_timer(NState)};
                _ ->
                    UMode = NState#client_state.user#user.mode ++ NMode,
                    helpers:send(State#client_state.socket, [":", NState#client_state.user#user.nick, " MODE ", NState#client_state.user#user.nick, " :+", NMode, "\r\n"]),
                    {next_state, ready, ping_pong:reset_timer(NState#client_state{user = NState#client_state.user#user{mode = UMode}})}
            end;

        false ->
            {next_state, ready, ping_pong:reset_timer(State)}
    end.

names(State, Chan) ->
    case gen_server:call(irckerl, {get_channel, Chan}) of
        {ok, Info} ->
            case gen_server:call(Info, get_users) of
                {ok, Users} ->
                    %Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),

                    Str = trim:trim(lists:map(fun(N) -> N#user.nick ++ " " end, Users)),
                    helpers:send(State, "353", [" @ ", Chan, " :", Str]);

                {error, Error} ->
                    ?ERROR("Error in get_users query for channel ~p: ~s~n", [Chan, Error])
            end;

        {error, Error} ->
            ?ERROR("Error in get_users query for channel ~p: ~s~n", [Chan, Error])
    end,

    helpers:send(State, "366", [Chan, " :End of NAMES list"]),
    {next_state, ready, ping_pong:reset_timer(State)}.


privmsg(State, To, Message) ->
    case utils:valid_channel(To) of
        true ->
            case gen_server:call(irckerl, {get_channel, To}) of
                {ok, Info} ->
                    case gen_server:call(Info, {privmsg, State#client_state.user#user.nick, irckerl_parser:full_nick(State#client_state.user), To, Message}) of
                        ok ->
                            ok;
                        {error, Error} ->
                            helpers:send(State, "437", [To, ":Could not send message ", Error]) % TODO: correct error code
                    end;

                {error, Error} ->
                    helpers:send(State, "437", [":Could not find the channel ", To, " ", Error]) % TODO: correct error code/message
            end;

        _ -> % TODO: get user and send message
            case gen_server:call(irckerl, {get_user, To}) of
                {ok, Info} ->
                    case gen_fsm:send_event(Info#user.pid, {privmsg, irckerl_parser:full_nick(State#client_state.user), To, Message}) of
                        ok ->
                            ok;
                        {error, Error} ->
                            helpers:send(State, "437", [To, ":Could not send message ", Error]) % TODO: correct error code
                    end;

                {error, Error} ->
                    helpers:send(State, "437", [To, ":Could not send message ", Error]) % TODO: correct error code
            end
    end,

    {next_state, ready, ping_pong:reset_timer(State)}.


% TODO: one can also query WHO w/o param (equals WHO 0) and WHO user and WHO pattern
who(State, Channel = "#" ++ _) ->
    case gen_server:call(irckerl, {get_channel, Channel}) of
        {ok, Info} ->
            case gen_server:call(Info, get_users) of
                {ok, Users} ->
                    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
                    lists:map(fun(User) ->
                                      helpers:send(State, "352", [
                                                          Channel, " ",
                                                          User#user.username, " ",
                                                          User#user.masked, " ",
                                                          Host, " ",
                                                          User#user.nick, " H :0 ",
                                                          User#user.realname
                                                         ]
                                          )
                              end, Users);

                {error, Error} ->
                    ?ERROR("Error in get_users query for channel ~p: ~s~n", [Channel, Error])
            end;

        {error, Error} ->
            ?ERROR("Error in get_users query for channel ~p: ~s~n", [Channel, Error])
    end,

    helpers:send(State, "315", [Channel, " :End of /WHO list."]),
    {next_state, ready, ping_pong:reset_timer(State)}.


ping(State, PingId, SName) ->
    helpers:send(State, ["PONG ", PingId]),
    {next_state, SName, ping_pong:reset_timer(State)}.


pong(Receiver, State, SName) ->
    case Receiver == proplists:get_value(hostname, State#client_state.settings, "localhost") of
        true ->
            T = (ping_pong:reset_timer(State))#client_state{ping_sent=false};
        _ ->
            case Receiver == State#client_state.no_spoof of
                true ->
                    T = (ping_pong:reset_timer(State))#client_state{ping_sent=false};
                _ ->
                    T = ping_pong:reset_timer(State)
            end
    end,
    {next_state, SName, T}.

%% eof
