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

-record(state, {name, mode, clients, settings}).
-record(client, {nick,user,host,flag,process,norm_nick}).

% API
-export([start_link/3, stop/0]).

%-export([register_client/1, get_connected_client_count/0]).
%-export([send/1, send/2, terminate/0, terminate/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).



start_link(Settings,Name,Mode) ->
    error_logger:info_msg("created channel ~p with mode ~p...~n",[Name, Mode]),

    case gen_server:start_link(?MODULE, [Settings, Name, Mode], []) of
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



init([Settings, Name, Mode]) ->
    process_flag(trap_exit, true),
    {ok, #state{name=Name, mode=Mode, settings=Settings, clients = []}}.


stop() ->
    gen_server:call(self(),stop).



handle_call({join,{Nick,User,Host},Pid}, _, State = #state{name=Chan}) ->
    Clients = State#state.clients ++ [#client{nick=Nick,user=User,host=Host,norm_nick=irckerl_parser:to_lower(Nick),process=Pid}],
    Names = lists:map(fun(_ = #client{nick=N,process=CPid}) ->
                              gen_fsm:send_event(CPid,{join,Nick++"!"++User++"@"++Host,Chan}),
                              N
                      end,State#state.clients),
    {reply, {ok, Names}, State#state{clients=Clients}};

handle_call({part,Nick}, _, State) ->
    LNick = irckerl_parser:to_lower(Nick),
    Clients = lists:filter(fun(_ = #client{nick=N}) -> N =/= LNick end, State#state.clients),
    {reply, ok, State#state{clients=Clients}};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(_, State) ->
    ?DEBUG("down with channel ~p~n",[State#state.name]),
    ok.

% eof