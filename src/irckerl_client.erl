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

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(gen_fsm).

-include_lib("kernel/include/inet.hrl").

-include("irckerl.hrl").
-include("umodes.hrl").
-include("cmodes.hrl").

-import(irc.client.helpers).
-import(irc.client.ping_pong).


%% entry points
-export([start/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: registering_nick -> registering_user -> ready
-export([registering_nick/2, registering_user/2, ready/2]).

start([Settings, Socket]) ->
    gen_fsm:start(?MODULE, [Settings, Socket], [{debug, trace}]).

start_link([Settings, Socket]) ->
    gen_fsm:start_link(?MODULE, [Settings, Socket], []).



init([Settings, Client]) ->
    process_flag(trap_exit, true),
    case gen_server:call(irckerl, {register_client, self()}) of
        ok ->
            case ping_pong:set_timer(Settings) of
                {ok, Timer} ->
                    State = #client_state{
                      socket = Client,
                      settings = Settings,
                      no_spoof = utils:random_str(8),
                      last_activity = erlang:now(),
                      the_timer = Timer,
                      user = #user{pid = self()}
                     },
                    {ok, registering_nick, State#client_state{user = get_user_info(State, Client)}};

                {error, Reason} ->
                    error_logger:error_msg("error when registering as a client: ~p~n", [Reason]),
                    {error, {timer_failed, Reason}};
                Other ->
                    error_logger:error_msg("error when registering as a client: ~p~n", [Other]),
                    {error, {other, Other}}
            end;

        Other ->
            error_logger:error_msg("error when registering as a client: ~p~n", [Other]),
            {error, not_accepted}
    end.


handle_info({tcp_closed, Socket}, SName, State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), quit),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _}, SName, State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event(self(), quit),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    Line = trim:trim(Data),

    io:format("R: ~p~n", [Line]),

    gen_fsm:send_event(self(), {received, Line}),
    {next_state, SName, State};

handle_info(ping, SName, State) ->
    gen_fsm:send_event(self(), ping),
    {next_state, SName, State};

handle_info({privmsg, From, To, Msg}, SName, State) ->
    gen_fsm:send_event(self(), {privmsg, From, To, Msg}),
    {next_state, SName, State};

handle_info(Info, SName, State) ->
    error_logger:error_msg("handle_info(~p, ~p, ~p) called! Should never happen...~n", [Info, SName, State]),
    {next_state, SName, State}.



handle_event(Ev, StateName, State) ->
    error_logger:error_msg("handle_event(~p, ~p, ~p) called! Should never happen...~n", [Ev, StateName, State]),
    {stop, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.



handle_sync_event(Ev, From, StateName, State) ->
    error_logger:error_msg("handle_sync_event(~p, ~p, ~p, ~p) called! Should never happen...~n", [Ev, From, StateName, State]),
    {stop, "WTF?! Don't use gen_fsm:sync_send_all_state_event, fucker!"}.



code_change(_, Name, State, _) ->
    {ok, Name, State}.



terminate(_Reason, _StateName, State) ->
    ?DEBUG("terminating client ~p~n", [State]), %#client_state.user#user.ip
    ok.



%%%%
%%%% states
%%%%

% TODO:
% registering_nick({received, <<"PASS ", Data/binary>>}, State) ->


registering_nick({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "NICK", [Nick]} ->
            case utils:valid_nick(Nick, State#client_state.settings) of
                valid ->
                    NormNick = irckerl_parser:to_lower(Nick),
                    case helpers:send_server({choose_nick, Nick, NormNick, State#client_state.user}) of
                        ok ->
                            NState = ping_pong:reset_timer(ping_pong:try_ping(prenick, State)),
                            Usr = NState#client_state.user,
                            {next_state, registering_user, NState#client_state{user = Usr#user{nick = Nick, normalized_nick = NormNick}}};

                        Other ->
                            ?DEBUG("Error: nick could not be reserved: ~p~n", [Other]),
                            helpers:send(State, "433", Nick, [":Nick already in use, choose another one"]),
                            {next_state, registering_nick, ping_pong:reset_timer(State)}
                    end;

                invalid ->
                    ?DEBUG("Error: invalid nick name ~p", [Nick]),
                    helpers:send(State, "432", Nick, [":Error in nick name, choose another one"]),
                    {next_state, registering_nick, ping_pong:reset_timer(State)}
            end;

        {ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event(self(), quit),
            {next_state, registering_nick, State};

        {ok, _Prefix, "PONG", [Ref]} ->
            case Ref == State#client_state.no_spoof of
                true ->
                    {next_state, registering_nick, (ping_pong:reset_timer(State))#client_state{ping_sent=false, no_spoof=utils:random_str(8)}};
                _ ->
                    {next_state, registering_nick, ping_pong:reset_timer(State)}
            end;

        {ok, _Prefix, Cmd, _} ->
            ?DEBUG("Error: registering_nick: unexpected data: ~p~n", [Data]),
            helpers:send(State, "451", Cmd, [":Register first!"]),
            {next_state, registering_nick, ping_pong:reset_timer(State)};

        _ ->
            ?DEBUG("Error: registering_nick: unexpected data: ~p~n", [Data]),
            helpers:send(State, "451", Data, [":Register first!"]),
            {next_state, registering_nick, ping_pong:reset_timer(State)}
    end;

registering_nick(quit, State) ->
    {stop, shutdown, State};
registering_nick(ping, State) ->
    {next_state, registering_nick, ping_pong:try_ping(prenick, State)};
registering_nick(What, State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n", [What]),
    {next_state, registering_nick, State}.



registering_user({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "USER", [Username, Mode, Unused, Realname]} -> % TODO: use Mode if specified correctly; what is Unused?
            Usr = State#client_state.user,
            NState = State#client_state{
                       user = Usr#user{username = Username, realname = Realname, mode = proplists:get_value(std_umode, State#client_state.settings, "iwx")}
                      },
            send_first_messages(NState),
            {next_state, ready, ping_pong:reset_timer(NState)};

        {ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event(self(), quit),
            {next_state, registering_user, ping_pong:reset_timer(State)};

        {ok, _Prefix, "PONG", [Receiver]} ->
            irc.client:pong(Receiver, State, registering_user);

        {ok, _Prefix, Cmd, _} ->
            ?DEBUG("Error: registering_user: unexpected data: ~p~n", [Data]),
            helpers:send(State, "451", Cmd, [":Register first!"]),
            {next_state, registering_user, ping_pong:reset_timer(State)};
        _ ->
            ?DEBUG("Error: registering_user: unexpected data: ~p~n", [Data]),
            helpers:send(State, "451", Data, [":Register first!"]),
            {next_state, registering_user, ping_pong:reset_timer(State)}
    end;

registering_user(quit, State) ->
    {stop, shutdown, State};
registering_user(ping, State) ->
    {next_state, registering_user, ping_pong:try_ping(State)};
registering_user(What, State) ->
    ?DEBUG("Got unknown event: ~p in state registering_user~n", [What]),
    {next_state, registering_user, State}.



ready({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "MODE", [Nick]} ->
            irc.client:mode(State, Nick);

        {ok, _Prefix, "MODE", [Nick, Mode]} ->
            irc.client:mode(State, Nick, Mode);

        {ok, _Prefix, "JOIN", [Chan]} ->
            irc.client:join(State, Chan);

        {ok, _Prefix, "WHO", [Pattern]} ->
            irc.client:who(State, Pattern);

        {ok, _Prefix, "NAMES", [Chan]} ->
            irc.client:names(State, Chan);

        {ok, _Prefix, "PRIVMSG", [Nick, Message]} -> % TODO: get channel and send message
            irc.client:privmsg(State, Nick, Message);

        {ok, _Prefix, "PING", [PingId]} ->
            irc.client:ping(State, PingId, ready);

        % TODO: implement forwarded pings
        %{ok, _Prefix, "PING", [PingId, To]} ->
        %    helpers:send(State, ["PONG ", PingId]),
        %    {next_state, ready, ping_pong:reset_timer(State)};

        {Ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event(self(), quit),
            {next_state, ready, ping_pong:reset_timer(State)};

        {ok, _Prefix, "PONG", [Receiver]} ->
            irc.client:pong(Receiver, State, ready);

        _ ->
            ?DEBUG("Error: ready: unexpected data: ~p~n", [Data]),
            helpers:send(State, "421", Data, [":Unknown command!"]),
            {next_state, ready, ping_pong:reset_timer(State)}

        end;

ready(ping, State) ->
    {next_state, ready, ping_pong:try_ping(State)};
ready({join, Nick, Chan}, State) ->
    helpers:send(State#client_state.socket, [":", Nick, " JOIN ", Chan, "\r\n"]),
    {next_state, ready, State};
ready({privmsg, From, To, Msg}, State) ->
    helpers:send(State#client_state.socket, [":", From, " PRIVMSG ", To, " :", Msg, "\r\n"]),
    {next_state, ready, State};
ready(quit, State) ->
    {stop, shutdown, State};
ready(What, State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n", [What]),
    {next_state, ready, State}.


%%%
%%% internal
%%%



send_first_messages(State) ->
    {created, {{Year, Month, Day}, {Hour, Minute, Second}}} = helpers:send_server(created),
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    Lim = proplists:get_value(limits, State#client_state.settings, []),
    Set = State#client_state.settings, % Set is much less to type

    {visible, Visible, invisible, Invisible} = helpers:send_server(count_users),
    {servers, Servers} = helpers:send_server(count_servers),

    helpers:send(State, "001", [":Welcome to the ", proplists:get_value(ircnetwork, Set, "ROXNet"), " IRC Network"]),
    helpers:send(State, "002", [":Your host is ", Host, ", running IRCKErl V", ?VERSION]),
    helpers:send(State, "003", [
                      ":This server was created at ",
                      integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), " ",
                      integer_to_list(Hour), ":", integer_to_list(Minute), ":", integer_to_list(Second)
                     ]),
    helpers:send(State, "004", [
                      Host,
                      " IRCKErl",
                      ?VERSION, " ",
                      lists:map(fun({Mode, _, _}) -> Mode end, ?UMODES), " ",
                      lists:map(fun({CMode, _}) -> CMode end, ?CMODES)
                     ]
        ), % TODO: send implemented modes
    MChan = integer_to_list(proplists:get_value(maxchannels, Lim, 10)),
    helpers:send(State, "005", [
                      "MAXCHANNELS=", MChan,
                      " CHANLIMIT=#:", MChan,
                      " NICKLEN=", integer_to_list(proplists:get_value(nicklen, Lim, 30)),
                      " CHANNELLEN=", integer_to_list(proplists:get_value(chanlen, Lim, 30)),
                      " TOPICLEN=", integer_to_list(proplists:get_value(topiclen, Lim, 300)),
                      " KICKLEN=", integer_to_list(proplists:get_value(kicklen, Lim, 300)),
                      " AWAYLEN=", integer_to_list(proplists:get_value(awaylen, Lim, 300)),
                      " MAXTARGETS=", integer_to_list(proplists:get_value(maxtargets, Lim, 20)),
                      " :are supported by this server"
                     ]),
    helpers:send(State, "005", ["NETWORK=", proplists:get_value(ircnetwork, Set, "ROXNet"), " CASEMAPPING=ascii :are supported by this server"]),
    helpers:send(State, "251", [":There are ", integer_to_list(Visible + Invisible), " and ", integer_to_list(Invisible), " users on ", integer_to_list(Servers), " servers"]),
    % TODO: send 255 :I have x clients and x servers
    % TODO: send 265 :Current Local Users: x  Max: x
    % TODO: send 266 :Current Global Users: x  Max: x
    case proplists:get_value(motd, Set, none) of
        none ->
            helpers:send(State, "422", [":MOTD file is missing"]);
        Filename ->
            case file:read_file(Filename) of
                {ok, Data} ->
                    helpers:send(State, "375", [":- ", proplists:get_value(ircnetwork, Set, "ROXNet"), " message of the day -"]),
                    lists:map(fun(Line) -> helpers:send(State, "372", [":- ", Line]) end, re:split(trim:trim(binary_to_list(Data)), "\r\n|\r|\n")),
                    helpers:send(State, "376", [":End of /MOTD command."]);

                _ ->
                    helpers:send(State, "422", [":MOTD file is missing"])
            end
    end,
    helpers:send(State#client_state.socket, [":", State#client_state.user#user.nick, " MODE ", State#client_state.user#user.nick, " :+", State#client_state.user#user.mode, "\r\n"]).


get_user_info(State, Sock) ->
    {ok, {Ip, _}} = inet:peername(Sock),

    helpers:send(State, "NOTICE", "AUTH", [":*** Looking up your hostname"]),

    case inet:gethostbyaddr(Ip) of
        {ok, HEnt} ->
            helpers:send(State, "NOTICE", "AUTH", [":Using hostname ", HEnt#hostent.h_name]),
            State#client_state.user#user{ip = Ip, host = HEnt#hostent.h_name, masked = utils:mask_host(HEnt#hostent.h_name)};

        _ ->
            helpers:send(State, "NOTICE", "AUTH", [":Couldn't resolve your hostname, using IP instead"]),
            State#client_state.user#user{ip = Ip, host = Ip, masked = utils:mask_ip(Ip)}
    end.




% eof
