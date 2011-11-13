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

-module(irckerl_channel).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(gen_server).

-include("irckerl.hrl").

% API
-export([start_link/3, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-import(irc.channel).


% @doc This module represents a IRC chanel to which you can
% join, part, send messages, etc. The irckerl server can
% handle thouthends of such chanels.

% @doc Starts a chanel process, returns a touple {ok, Server} or
% {error, Reason} if this process could not have been started.
-spec start_link(proplist(), string(), string()) -> {ok, pid()} | {error, _}.
start_link(Settings,Name,Mode) ->
    error_logger:info_msg("created channel ~p with mode ~p...~n",[Name, Mode]),

    case gen_server:start_link(?MODULE, {Settings, Name, Mode}, []) of
        {ok, Server} ->
            error_logger:info_msg("gen_server:start_link was successful in channel module for channel ~p~n",[Name]),
            {ok, Server};

        {error, {already_started, Server}} ->
            error_logger:info_msg("gen_server:start_link was error: already_started in channel module for channel ~p~n",[Name]),
            {ok, Server};

        {error, Reason} ->
            error_logger:error_msg("Error starting channel ~p: ~w~n",[Reason]),
            {error, Reason}
    end.


-spec init({proplist(), string(), string()}) -> {ok, #channel_state{}}.
init({Settings, Name, Mode}) ->
  process_flag(trap_exit, true),
  {ok, #channel_state{
      channel  = #channel {
        name            = Name,
        normalized_name = irckerl_parser:to_lower(Name),
        mode            = Mode,
        topic           = "",
        members         = [],
        pid             = self()
      },

      settings = Settings
    }
  }.

-spec stop() -> any().
stop() ->
    gen_server:call(self(),stop).


-spec handle_call(term(), _, #channel_state{}) -> {reply, term(), #channel_state{}}.
handle_call({join, User}, _, State = #channel_state{channel=Chan}) ->
    channel:join(State, Chan, User);

handle_call({part,Nick}, _, State = #channel_state{channel=Chan}) ->
    channel:part(State, Chan, Nick);

handle_call({privmsg, Nick, From, To, Message}, _, State = #channel_state{channel=Chan}) ->
    channel:privmsg(State, Chan, Nick, From, To, Message);

handle_call(topic, _, State = #channel_state{channel=Chan}) ->
    {reply, {ok, Chan#channel.topic}, State};
handle_call({topic, Topic, Author}, _, State = #channel_state{channel = Chan}) ->
    NTop = #topic{topic = Topic, updated = erlang:localtime(), author = Author},
    channel:send_messages(Chan#channel.members, {msg, [":", irckerl_parser:full_nick(Author), " TOPIC ", Chan#channel.name, " :", Topic, "\r\n"]}),
    {reply, ok, State#channel_state{channel=Chan#channel{topic = NTop}}};

handle_call(get_users, _, State = #channel_state{channel = Chan}) ->
    channel:users(State, Chan);

handle_call(P1, P2, State) ->
    io:format("called: handle_call(~p,~p,~p)~n",[P1,P2,State]),
    {reply, ok, State}.

-spec handle_cast(_, #channel_state{}) -> {noreply, #channel_state{}}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(_, #channel_state{}) -> {noreply, #channel_state{}}.
handle_info(_, State) ->
    {noreply, State}.

-spec code_change(term(), #channel_state{}, term()) -> {ok, #channel_state{}}.
code_change(_, State, _) ->
    {ok, State}.

-spec terminate(normal | shutdown | term(), #channel_state{}) -> ok.
terminate(_, State) ->
    ?DEBUG("down with channel ~p~n",[State#channel_state.channel#channel.name]),
    ok.




% eof
