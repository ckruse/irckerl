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

-module(irckerl_client).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-behaviour(gen_fsm).

-include_lib("kernel/include/inet.hrl").

-include("irckerl.hrl").
-include("umodes.hrl").
-include("cmodes.hrl").


%% entry points
-export([start/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: registering_nick -> registering_user -> ready
-export([registering_nick/2, registering_user/2, ready/2]).

-spec start({proplist(), inet:socket()}) -> {ok, pid()} | {error, term() | {already_started, pid()}}.
start({Settings, Socket}) ->
    gen_fsm:start(?MODULE, {Settings, Socket}, [{debug, [trace]}]).

-spec start_link({proplist(), inet:socket()}) -> {ok, pid()} | {error, term() | {already_started, pid()}}.
start_link({Settings, Socket}) ->
    gen_fsm:start_link(?MODULE, {Settings, Socket}, [{debug, [trace]}]).


-spec init({proplist(), inet:socket()}) -> {ok, registering_nick, #client_state{}} | {stop, not_accepted | {timer_failed, term()}}.
init({Settings, Client}) ->
    process_flag(trap_exit, true),
    case gen_server:call(irckerl_controller, {register_client, self()}) of
        ok ->
            case irc_client_ping_pong:set_timer(Settings) of
                {ok, Timer} ->
                    State = #client_state{
                        socket        = Client,
                        settings      = Settings,
                        no_spoof      = irckerl_utils:random_str(8),
                        last_activity = erlang:now(),
                        the_timer     = Timer,
                        user          = #user{pid = self()}
                    },
                    {ok, registering_nick, State#client_state{user = get_user_info(State, Client)}};

                {error, Reason} ->
                    ?ERROR("error when registering as a client: ~p", [Reason]),
                    {stop, {timer_failed, Reason}}
            end;

        Other ->
            ?ERROR("error when registering as a client: ~p", [Other]),
            {stop, not_accepted}
    end.

-spec handle_info(
  {tcp_error, inet:socket(), string()} |
    {tcp_closed, inet:socket()} |
    {tcp, inet:socket(), binary()} |
    ping |
    {privmsg, string(), string(), string()},
  atom(),
  #client_state{}
) -> {next_state, atom(), #client_state{}}.

handle_info({tcp_closed, Socket}, SName, State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), {quit, "Connection reset by peer"}),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _}, SName, State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), {quit, "Connection reset by peer"}),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    Line = trim:trim(Data),

    ?DEBUG("R: ~p", [Line]),

    gen_fsm:send_event(self(), {received, Line}),
    {next_state, SName, State};

handle_info(ping, SName, State) ->
    gen_fsm:send_event(self(), ping),
    {next_state, SName, State};

handle_info({privmsg, From, To, Msg}, SName, State) ->
    gen_fsm:send_event(self(), {privmsg, From, To, Msg}),
    {next_state, SName, State};

handle_info(Info, SName, State) ->
    ?ERROR("handle_info(~p, ~p, ~p) called! Should never happen...", [Info, SName, State]),
    {next_state, SName, State}.


handle_event(Ev, StateName, State) ->
    ?ERROR("handle_event(~p, ~p, ~p) called! Should never happen...", [Ev, StateName, State]),
    {stop, error, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.



handle_sync_event(Ev, From, StateName, State) ->
    ?ERROR("handle_sync_event(~p, ~p, ~p, ~p) called! Should never happen...", [Ev, From, StateName, State]),
    {stop, error, "WTF?! Don't use gen_fsm:sync_send_all_state_event, fucker!"}.


-spec code_change(term(), atom(), #client_state{}, term()) -> {ok, atom(), #client_state{}}.
code_change(_, Name, State, _) ->
    {ok, Name, State}.


-spec terminate(normal | shutdown | term(), atom(), #client_state{}) -> ok.
terminate(_Reason, _StateName, State) ->
    gen_tcp:close(State#client_state.socket),
    ?DEBUG("terminating client ~p~n", [State]), %#client_state.user#user.ip
    ok.



%%%%
%%%% states
%%%%

% TODO:
% registering_nick({received, <<"PASS ", Data/binary>>}, State) ->

-spec registering_nick(
  {received, binary()} |
    quit |
    ping,
  #client_state{}
) -> {next_state, registering_nick, #client_state{}}.
registering_nick({received, Data}, State) ->
    case irc_parser:parse(Data) of
        {ok, #irc_cmd{cmd = "NICK", params = [[Nick]]}} ->
            irc_client:nick(State, Nick);

        {ok, #irc_cmd{cmd = "QUIT"}} ->
            gen_fsm:send_event(self(), quit),
            {next_state, registering_nick, State};

        {ok, #irc_cmd{cmd = "PONG", params = [[Ref]]}} ->
            case Ref == State#client_state.no_spoof of
                true ->
                    {next_state, registering_nick, (irc_client_ping_pong:reset_timer(State))#client_state{ping_sent=false, no_spoof=irckerl_utils:random_str(8)}};
                _ ->
                    {next_state, registering_nick, irc_client_ping_pong:reset_timer(State)}
            end;

        {ok, #irc_cmd{cmd = Cmd}} ->
            ?DEBUG("Error: registering_nick: unexpected data: ~p~n", [Data]),
            irc_client_helpers:send(State, "451", Cmd, [":Register first!"]),
            {next_state, registering_nick, irc_client_ping_pong:reset_timer(State)};

        _ ->
            ?DEBUG("Error: registering_nick: unexpected data: ~p~n", [Data]),
            irc_client_helpers:send(State, "451", binary_to_list(Data), [":Register first!"]),
            {next_state, registering_nick, irc_client_ping_pong:reset_timer(State)}
    end;

registering_nick(quit, State) ->
    {stop, shutdown, State};
registering_nick(ping, State) ->
    {next_state, registering_nick, irc_client_ping_pong:try_ping(prenick, State)};
registering_nick(What, State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n", [What]),
    {next_state, registering_nick, State}.



registering_user({received, Data}, State) ->
    case irc_parser:parse(Data) of
        {ok, #irc_cmd{cmd = "USER", params = [[Username], [Param1], [Param2], [Realname]]}} ->
            irc_client:user(State, Username, Param1, Param2, Realname);

        {ok, #irc_cmd{cmd = "QUIT"}} ->
            gen_fsm:send_event(self(), quit),
            {next_state, registering_user, irc_client_ping_pong:reset_timer(State)};

        {ok, #irc_cmd{cmd = "PONG", params = [[Receiver]]}} ->
            irc_client:pong(State, registering_user, Receiver);

        {ok, #irc_cmd{cmd = Cmd}} ->
            ?DEBUG("Error: registering_user: unexpected data: ~p~n", [Data]),
            irc_client_helpers:send(State, "451", Cmd, [":Register first!"]),
            {next_state, registering_user, irc_client_ping_pong:reset_timer(State)};

        _ ->
            ?DEBUG("Error: registering_user: unexpected data: ~p~n", [Data]),
            irc_client_helpers:send(State, "451", binary_to_list(Data), [":Register first!"]),
            {next_state, registering_user, irc_client_ping_pong:reset_timer(State)}
    end;

registering_user(quit, State) ->
    {stop, shutdown, State};
registering_user(ping, State) ->
    {next_state, registering_user, irc_client_ping_pong:try_ping(State)};
registering_user(What, State) ->
    ?DEBUG("Got unknown event: ~p in state registering_user~n", [What]),
    {next_state, registering_user, State}.



ready({received, Data}, State) ->
    case irc_parser:parse(Data) of
        {ok, #irc_cmd{cmd = "MODE", params = [[Nick]]}} ->
            irc_client:mode(State, Nick);

        {ok, #irc_cmd{cmd = "MODE", params = [[Nick], [Mode]]}} ->
            irc_client:mode(State, Nick, Mode);

        {ok, #irc_cmd{cmd = "JOIN", params = [["0"]]}} ->
            irc_client:join(State, "0");
        {ok, #irc_cmd{cmd = "JOIN", params = [Channels]}} -> % channels come in as chan1[,chan2[,chan3[,...]]] so [Channels] matches always
            irc_client:join(State, Channels);

        {ok, #irc_cmd{cmd = "JOIN", params = [Channels, Passwords]}} -> % channels come in as chan1[,chan2[,chan3[,...]]] so [Channels] matches always
            irc_client:join(State, Channels, Passwords);

        {ok, #irc_cmd{cmd = "PART", params = [[Args]]}} ->
            irc_client:part(State, Args);

        {ok, #irc_cmd{cmd = "WHO", params = [[Pattern]]}} ->
            irc_client:who(State, Pattern);

        {ok, #irc_cmd{cmd = "NAMES", params = [[Chan]]}} ->
            irc_client:names(State, Chan);

        {ok, #irc_cmd{cmd = "PRIVMSG", params = [[Nick], [Message]]}} ->
            irc_client:privmsg(State, Nick, Message);

        {ok, #irc_cmd{cmd = "PING", params = [[PingId]]}} ->
            irc_client:ping(State, ready, PingId);

        % TODO: implement forwarded pings
        %{ok, _Prefix, "PING", [PingId, To]} ->
        %    irc_client_helpers:send(State, ["PONG ", PingId]),
        %    {next_state, ready, irc_client_ping_pong:reset_timer(State)};

        {ok, #irc_cmd{cmd = "TOPIC", params = [[Channel], [Topic]]}} ->
            irc_client:topic(State, Channel, Topic);
        {ok, #irc_cmd{cmd = "TOPIC", params = [[Channel]]}} ->
            irc_client:topic(State, Channel);
        {ok, #irc_cmd{cmd = "TOPIC", params = P}} ->
            ?DEBUG("Params: ~p",[P]),
            irc_client_helpers:send(State, "461", "TOPIC :Not enough parameters"),
            {next_state, ready, irc_client_ping_pong:reset_timer(State)};

        {ok, #irc_cmd{cmd = "QUIT", params = [[Reason]]}} ->
            gen_fsm:send_event(self(), {quit, Reason}),
            {next_state, ready, irc_client_ping_pong:reset_timer(State)};

        {ok, #irc_cmd{cmd = "QUIT"}} ->
            gen_fsm:send_event(self(), {quit, "Quitting for user request"}),
            {next_state, ready, irc_client_ping_pong:reset_timer(State)};

        {ok, #irc_cmd{cmd = "PONG", params = [[Receiver]]}} ->
            irc_client:pong(State, ready, Receiver);

        _ ->
            ?DEBUG("Error: ready: unexpected data: ~p~n", [Data]),
            irc_client_helpers:send(State, "421", binary_to_list(Data), [":Unknown command!"]),
            {next_state, ready, irc_client_ping_pong:reset_timer(State)}

        end;

ready(ping, State) ->
    {next_state, ready, irc_client_ping_pong:try_ping(State)};
ready({join, Nick, Chan}, State) ->
    irc_client_helpers:send(State#client_state.socket, [":", Nick, " JOIN ", Chan, "\r\n"]),
    {next_state, ready, State};
ready({privmsg, From, To, Msg}, State) ->
    irc_client_helpers:send(State#client_state.socket, [":", From, " PRIVMSG ", To, " :", Msg, "\r\n"]),
    {next_state, ready, State};
ready({msg, Data}, State) ->
    irc_client_helpers:send(State#client_state.socket, Data),
    {next_state,ready, State};
ready({quit, Reason}, State) ->
    lists:map(fun(C) ->
            gen_server:cast(C#channel.pid, {quit, State#client_state.user, Reason})
        end, State#client_state.channels),
    {stop, shutdown, State};
ready(What, State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n", [What]),
    {next_state, ready, State}.


%%%
%%% internal
%%%



-spec get_user_info(#client_state{}, inet:socket()) -> #user{}.
get_user_info(State, Sock) ->
    {ok, {Ip, _}} = inet:peername(Sock),

    irc_client_helpers:send(State, "NOTICE", "AUTH", [":*** Looking up your hostname"]),

    case inet:gethostbyaddr(Ip) of
        {ok, HEnt} ->
            irc_client_helpers:send(State, "NOTICE", "AUTH", [":Using hostname ", HEnt#hostent.h_name]),
            State#client_state.user#user{ip = Ip, host = HEnt#hostent.h_name, masked = irckerl_utils:mask_host(HEnt#hostent.h_name)};

        _ ->
            irc_client_helpers:send(State, "NOTICE", "AUTH", [":Couldn't resolve your hostname, using IP instead"]),
            State#client_state.user#user{ip = Ip, host = Ip, masked = irckerl_utils:mask_ip(Ip)}
    end.




% eof
