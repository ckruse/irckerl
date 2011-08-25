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

-record(state, {
          nick, normalized_nick, socket,
          settings, user_info, no_spoof,
          the_timer, last_activity, ping_sent,
          umode
         }
       ).

%% entry points
-export([start/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: registering_nick -> registering_user -> ready
-export([registering_nick/2, registering_user/2, ready/2]).

start([Settings, Socket]) ->
    gen_fsm:start(?MODULE, [Settings, Socket], [{debug,trace}]).

start_link([Settings, Socket]) ->
    gen_fsm:start_link(?MODULE, [Settings, Socket], []).



init([Settings, Client]) ->
    process_flag(trap_exit, true),
    case gen_server:call(irckerl,{register_client, self()}) of
        ok ->
            case set_timer(Settings) of
                {ok, Timer} ->
                    State = #state{
                      nick = undefined,
                      socket = Client,
                      settings = Settings,
                      no_spoof = utils:random_str(8),
                      last_activity = erlang:now(),
                      the_timer = Timer
                     },
                    {ok,registering_nick,State#state{user_info = get_user_info(State,Client)}};

                {error, Reason} ->
                    error_logger:error_msg("error when registering as a client: ~p~n",[Reason]),
                    {error, {timer_failed,Reason}};
                Other ->
                    error_logger:error_msg("error when registering as a client: ~p~n",[Other]),
                    {error, {other, Other}}
            end;

        Other ->
            error_logger:error_msg("error when registering as a client: ~p~n",[Other]),
            {error, not_accepted}
    end.


handle_info({tcp_closed, Socket},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0, quit),
    {next_state, SName, State};

handle_info({tcp_error, Socket, _},SName,State) ->
    gen_tcp:close(Socket),
    gen_fsm:send_event_after(0, quit),
    {next_state, SName, State};

handle_info({tcp, _Socket, Data}, SName, State) ->
    Line = trim:trim(Data),

    io:format("R: ~p~n", [Line]),

    gen_fsm:send_event(self(), {received, Line}),
    {next_state, SName, State};

handle_info(ping, SName, State) ->
    gen_fsm:send_event(self(),ping),
    {next_state, SName, State};

handle_info(Info, SName, State) ->
    error_logger:error_msg("handle_info(~p, ~p, ~p) called! Should never happen...~n",[Info, SName, State]),
    {next_state, SName, State}.



handle_event(Ev, StateName, State) ->
    error_logger:error_msg("handle_event(~p, ~p, ~p) called! Should never happen...~n",[Ev,StateName,State]),
    {stop, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.



handle_sync_event(Ev, From, StateName, State) ->
    error_logger:error_msg("handle_sync_event(~p, ~p, ~p, ~p) called! Should never happen...~n",[Ev,From, StateName, State]),
    {stop, "WTF?! Don't use gen_fsm:sync_send_all_state_event, fucker!"}.



code_change(_, Name, State, _) ->
    {ok, Name, State}.



terminate(_Reason, _StateName, State) ->
    ?DEBUG("terminating client ~p~n",[proplists:get_value(ip,State#state.user_info,{127,0,0,1})]),
    cast_server({delete_nick,State#state.normalized_nick}),
    ok.



%%%%
%%%% states
%%%%

% TODO:
% registering_nick({received, <<"PASS ",Data/binary>>},State) ->


registering_nick({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "NICK",[Nick]} ->
            case utils:valid_nick(Nick,State#state.settings) of
                valid ->
                    NormNick = irckerl_parser:normalize_nick(Nick),
                    case send_server({reserve_nick,Nick,NormNick,self()}) of
                        ok ->
                            NState = reset_timer(try_ping(prenick,State)),
                            {next_state, registering_user, NState#state{nick = Nick, normalized_nick = NormNick}};

                        Other ->
                            ?DEBUG("Error: nick could not be reserved: ~p",[Other]),
                            send(State,"433",Nick,[":Nick already in use, choose another one"]),
                            {next_state, registering_nick, reset_timer(State)}
                    end;

                invalid ->
                    ?DEBUG("Error: invalid nick name ~p",[Nick]),
                    send(State,"432",Nick,[":Error in nick name, choose another one"]),
                    {next_state, registering_nick, reset_timer(State)}
            end;

        {ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, registering_nick, State};

        {ok, _Prefix, "PONG", [Ref]} ->
            case Ref == State#state.no_spoof of
                true ->
                    {next_state, registering_nick, (reset_timer(State))#state{ping_sent=false,no_spoof=utils:random_str(8)}};
                _Other ->
                    {next_state, registering_nick, reset_timer(State)}
            end;

        {ok, _Prefix, Cmd, _} ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", Cmd,[":Register first!"]),
            {next_state, registering_nick, reset_timer(State)};

        _Other ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", Data,[":Register first!"]),
            {next_state, registering_nick, reset_timer(State)}
    end;

registering_nick(quit,State) ->
    {stop, shutdown, State};
registering_nick(ping, State) ->
    {next_state, registering_nick, try_ping(prenick,State)};
registering_nick(What,State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n",[What]),
    {next_state, registering_nick, State}.



registering_user({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "USER", [Username,Mode,Unused,Realname]} ->
            UInfo = State#state.user_info,
            NState = State#state{
                       user_info = UInfo ++ [{user,Username},{mode,Mode},{unused,Unused},{realname,Realname}],
                       umode = proplists:get_value(std_umode,State#state.settings,"iwx")
                      },
            send_first_messages(NState),
            {next_state, ready, reset_timer(NState)};

        {ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, registering_user, reset_timer(State)};

        {ok, _Prefix, "PONG", [Receiver]} ->
            {next_state, registering_user, handle_pong(Receiver,State)};

        {ok, _Prefix, Cmd, _} ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", Cmd, [":Register first!"]),
            {next_state, registering_user, reset_timer(State)};
        _Other ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "451", Data, [":Register first!"]),
            {next_state, registering_user, reset_timer(State)}
    end;

registering_user(quit, State) ->
    {stop, shutdown, State};
registering_user(ping, State) ->
    {next_state, registering_user, try_ping(State)};
registering_user(What,State) ->
    ?DEBUG("Got unknown event: ~p in state registering_user~n",[What]),
    {next_state, registering_user, State}.



ready({received, Data}, State) ->
    case irckerl_parser:parse(Data) of
        {ok, _Prefix, "MODE",[Nick]} ->
            case irckerl_parser:normalize_nick(Nick) == State#state.normalized_nick of
                true ->
                    send(State,"421",[State#state.nick," +", State#state.umode]),
                    {next_state, ready, reset_timer(State)};
                _Other ->
                    {next_state, ready, reset_timer(State)}
            end;
        {ok, _Prefix, "MODE",[Nick, "+" ++ Mode]} ->
            case irckerl_parser:normalize_nick(Nick) == State#state.normalized_nick of
                true ->
                    NMode = lists:filter(
                              fun(X) ->
                                      lists:all(fun(Y) when Y =/= X, X =/= 'o', X =/= 'O' -> true;
                                                   (_) -> false
                                                end, State#state.umode)
                              end,Mode),
                    case NMode of
                        [] ->
                            {next_state, ready, reset_timer(State)};
                        _Other ->
                            UMode = State#state.umode ++ NMode,
                            send(State#state.socket,[":",State#state.nick, " MODE ",State#state.nick," :+", NMode,"\r\n"]),
                            {next_state, ready, reset_timer(State#state{umode = UMode})}
                    end;

                false ->
                    {next_state, ready, reset_timer(State)}
            end;

        {ok, _Prefix, "PING", [Host]} ->
            case Host == proplists:get_value(hostname,State#state.settings,"localhost") of
                true -> send(State,["PONG ",Host," :", Host]);
                _Other -> ok % TODO: send ping to other host
            end,
            {next_state, ready, reset_timer(State)};

        {ok, _Prefix, "QUIT", _} ->
            gen_fsm:send_event_after(0, quit),
            {next_state, ready, reset_timer(State)};
        {ok, _Prefix, "PONG", [Receiver]} ->
            {next_state, ready, handle_pong(Receiver,State)};
        _Other ->
            ?DEBUG("Error: unexpected data: ~p~n",[Data]),
            send(State, "421", Data, [":Unknown command!"]),
            {next_state, ready, reset_timer(State)}
        end;

ready(ping, State) ->
    {next_state, registering_user, try_ping(State)};
ready(quit, State) ->
    {stop, shutdown, State};
ready(What, State) ->
    ?DEBUG("Got unknown event: ~p in state ready~n",[What]),
    {next_state, ready, State}.


%%%
%%% internal
%%%

send(State, To, Code, Data) ->
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    send(State#state.socket, [":", Host, " ", Code, " ", To, " ", Data, "\r\n"]).

send(State, Code, Data) ->
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    send(State#state.socket, [":", Host, " ", Code, " ", State#state.nick, " ", Data, "\r\n"]).

send(State, Data) when is_tuple(State) ->
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    send(State#state.socket, [":", Host, " ", Data, "\r\n"]);

send(Sock,Msg) ->
    io:format("S: ~p~n",[Msg]),
    gen_tcp:send(Sock,Msg).


send_server(What) ->
    gen_server:call(irckerl,What).

cast_server(What) ->
    gen_server:cast(irckerl,What).


set_timer(Settings) ->
    timer:send_after(proplists:get_value(pingfreq,Settings,10) * 1000,ping).


reset_timer(State) ->
    case timer:cancel(State#state.the_timer) of
        {ok, cancel} ->
            case set_timer(State#state.settings) of
                {ok, TRef} ->
                    State#state{the_timer = TRef, last_activity = erlang:now()};

                {error, Reason} ->
                    error_logger:error_msg("Error creating timer: ~p",[Reason]),
                    State#state{last_activity = erlang:now()}
            end;

        {error, Reason} ->
            error_logger:error_msg("Error canceling timer: ~p",[Reason]),
            State#state{last_activity = erlang:now()}
    end.


try_ping(State) ->
    try_ping(State, proplists:get_value(hostname,State#state.settings,"localhost")).
try_ping(prenick,State) ->
    try_ping(State, State#state.no_spoof);

try_ping(State, What) ->
    case State#state.ping_sent of
        true ->
            send(State#state.socket,["ERROR :Connection timed out\r\n"]),
            gen_fsm:send_event_after(0,quit),
            NState = State#state{the_timer=undefined};

        _Other ->
            send(State#state.socket,["PING :", What, "\r\n"]),
            case set_timer(State#state.settings) of
                {ok, TRef} ->
                    NState = State#state{the_timer=TRef,ping_sent=true};
                {error,Reason} ->
                    error_logger:error_msg("Error creating timer: ~p",[Reason]),
                    NState = State#state{the_timer=undefined,ping_sent=true}
            end
    end,
    NState#state{last_activity=erlang:now()}.

handle_pong(Receiver,State) ->
    case Receiver == proplists:get_value(hostname,State#state.settings,"localhost") of
        true ->
            (reset_timer(State))#state{ping_sent=false};
        _Other ->
            reset_timer(State)
    end.


send_first_messages(State) ->
    {created, {{Year,Month,Day},{Hour,Minute,Second}}} = send_server(created),
    Host = proplists:get_value(hostname,State#state.settings,"localhost"),
    Lim = proplists:get_value(limits,State#state.settings,[]),
    Set = State#state.settings, % Set is much less to type
    {visible,Visible,invisible,Invisible} = send_server(count_users),
    {servers,Servers} = send_server(count_servers),

    send(State,"001",[":Welcome to the ",proplists:get_value(ircnetwork,Set,"ROXNet"), " IRC Network"]),
    send(State,"002",[":Your host is ", Host, ", running IRCKErl V", ?VERSION]),
    send(State,"003",[
                      ":This server was created at ",
                      integer_to_list(Year),"-",integer_to_list(Month),"-",integer_to_list(Day)," ",
                      integer_to_list(Hour),":",integer_to_list(Minute),":",integer_to_list(Second)
                     ]),
    send(State,"004",[Host, " IRCKErl", ?VERSION, " iowghraAsORTVSxNCWqBzvdHtGp lvhopsmntikrRcaqOALQbSeIKVfMCuzNTGj"]), % TODO: send implemented modes
    MChan = integer_to_list(proplists:get_value(maxchannels,Lim,10)),
    send(State,"005",[
                      "MAXCHANNELS=",MChan,
                      " CHANLIMIT=#:",MChan,
                      " NICKLEN=",integer_to_list(proplists:get_value(nicklen,Lim,30)),
                      " CHANNELLEN=",integer_to_list(proplists:get_value(chanlen,Lim,30)),
                      " TOPICLEN=",integer_to_list(proplists:get_value(topiclen,Lim,300)),
                      " KICKLEN=",integer_to_list(proplists:get_value(kicklen,Lim,300)),
                      " AWAYLEN=",integer_to_list(proplists:get_value(awaylen,Lim,300)),
                      " MAXTARGETS=",integer_to_list(proplists:get_value(maxtargets,Lim,20)),
                      " :are supported by this server"
                     ]),
    send(State,"005",["NETWORK=",proplists:get_value(ircnetwork,Set,"ROXNet")," CASEMAPPING=ascii :are supported by this server"]),
    send(State,"251",[":There are ",integer_to_list(Visible + Invisible)," and ", integer_to_list(Invisible), " users on ",integer_to_list(Servers), " servers"]),
    % TODO: send 255 :I have x clients and x servers
    % TODO: send 265 :Current Local Users: x  Max: x
    % TODO: send 266 :Current Global Users: x  Max: x
    case proplists:get_value(motd,Set,none) of
        none ->
            send(State,"422", [":MOTD file is missing"]);
        Filename ->
            case file:read_file(Filename) of
                {ok, Data} ->
                    send(State,"375",[":- ",proplists:get_value(ircnetwork,Set,"ROXNet")," message of the day -"]),
                    lists:map(fun(Line) -> send(State,"372",[":- ",Line]) end, re:split(trim:trim(binary_to_list(Data)),"\r\n|\r|\n")),
                    send(State,"376", [":End of /MOTD command."]);

                _Other ->
                    send(State,"422", [":MOTD file is missing"])
            end
    end,
    send(State#state.socket,[":", State#state.nick, " MODE ", State#state.nick," :+",State#state.umode,"\r\n"]).


get_user_info(State,Sock) ->
    {ok,{Ip,_}} = inet:peername(Sock),

    send(State,"NOTICE", "AUTH",[":*** Looking up your hostname"]),

    case inet:gethostbyaddr(Ip) of
        {ok, HEnt} ->
            send(State,"NOTICE", "AUTH",[":Using hostname ",HEnt#hostent.h_name]),
            [{ip,Ip},{host,HEnt#hostent.h_name},{masked,utils:mask_host(HEnt#hostent.h_name)}];

        _Other ->
            send(State,"NOTICE", "AUTH",[":Couldn't resolve your hostname, using IP instead"]),
            [{ip,Ip},{host,Ip},utils:mask_ip(Ip)]
    end.





% eof
