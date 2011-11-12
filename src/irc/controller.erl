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

-module(irc.controller).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-include("../irckerl.hrl").

-export([get_user/2, get_channel/2, choose_nick/4, join/3, delete_nick/2]).

-import(gen_fsm).
-import(gen_server).
-import(proplists).
-import(lists).
-import(dict).

-import(irckerl_parser).
-import(irckerl_channel).

-spec get_user(#controller_state{}, string()) -> {reply, {ok, #user{}}, #controller_state{}} | {reply, {error, _}, #controller_state	{}}.
get_user(State = #controller_state{reserved_nicks = RNicks}, Nick) ->
    NNick = irckerl_parser:to_lower(Nick),
    case dict:find(NNick, RNicks) of
        {ok, [User]} ->
            {reply, {ok, User}, State};
        Error ->
            ?ERROR("Error: could not find user ~p: ~p",[Nick, Error]),
            {reply, {error, Error}, State}
    end.

-spec get_channel(#controller_state{}, string()) -> {reply, {ok, pid()} | {error, _}, #controller_state{}}.
get_channel(State = #controller_state{channels = Channels}, Channel) ->
    NChan = irckerl_parser:to_lower(Channel),
    case dict:find(NChan, Channels) of
        {ok, [Pid]} ->
            {reply, {ok, Pid}, State};
        Error ->
            ?ERROR("Error: channel ~p not found: ~p~n",[Channel, Error]),
            {reply, {error, Error}, State}
    end.

-spec choose_nick(#controller_state{}, string(), string(), #user{}) -> {reply, ok | nick_registered_already, #controller_state{}}.
choose_nick(State = #controller_state{reserved_nicks = RNicks, clients = Clients}, Nick, NormNick, User) ->
    case dict:find(NormNick, RNicks) of
        {ok, _} ->
            {reply, nick_registered_already, State};
        _ ->
            NClients = lists:filter(fun(#user{pid = Pid}) -> Pid =/= User#user.pid end,Clients),
            NUser = User#user{nick=Nick, normalized_nick=NormNick},
            {reply, ok, State#controller_state{reserved_nicks = dict:append(NormNick, NUser, RNicks), clients = NClients ++ [NUser]}}
    end.

-spec join(#controller_state{}, string(), #user{}) -> {reply, {ok, [string()]} | {error, _} | {error, _, _}, #controller_state{}}.
join(State = #controller_state{channels = Channels, settings = Settings}, Channel, User) ->
    NChan = irckerl_parser:to_lower(Channel),
    case dict:find(NChan, Channels) of
        {ok, [Pid]} ->
            join_channel(Pid, State, User, Channels);

        _ ->
            case irckerl_channel:start_link(Settings, Channel, proplists:get_value(std_cmodes, Settings, [])) of
                {ok, Pid} ->
                    NChannels = dict:append(NChan,Pid,Channels),
                    join_channel(Pid, State, User, NChannels);
                Error ->
                    ?ERROR("Error creating channel ~p: ~p~n",[Channel,Error]),
                    {reply, {error, Error}, State}
            end
    end.


-spec join_channel(pid(), #controller_state{}, #user{}, dict()) -> {reply, term(), #controller_state{}}.
join_channel(Chan,State,User,Chans) ->
    case gen_server:call(Chan,{join,User}) of
        {ok, Names} ->
            {reply, {ok, Names}, State#controller_state{channels = Chans}};
        {error, Error} ->
            {reply, {error, Error}, State};
        Other ->
            {reply, {error, unexpected_error, Other}, State}
    end.

delete_nick(State = #controller_state{reserved_nicks = RNicks}, NormNick) ->
    case dict:find(NormNick, RNicks) of
        {ok, _} ->
            {noreply, State#controller_state{reserved_nicks = dict:erase(NormNick,RNicks)}};
        _ ->
            {noreply, State}
    end.

% eof