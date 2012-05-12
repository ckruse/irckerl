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

-module(irc_client).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-export([nick/2, user/5, names/2, mode/3, mode/2, join/2, join/3, privmsg/3, who/2, ping/3, pong/3, topic/2, topic/3, part/2, part/3, kick/4, version/1]).

-include("irckerl.hrl").

-spec nick(#client_state{}, string()) -> {next_state, registering_nick, #client_state{}}.
nick(State, Nick) ->
    case irc_utils:valid_nick(Nick, State#client_state.settings) of
        valid ->
            NormNick = irc_utils:to_lower(Nick),
            case irc_client_helpers:send_server({choose_nick, Nick, NormNick, State#client_state.user}) of
                ok ->
                    NState = irc_client_ping_pong:reset_timer(irc_client_ping_pong:try_ping(prenick, State)),
                    Usr = NState#client_state.user,
                    {next_state, registering_user, NState#client_state{user = Usr#user{nick = Nick, normalized_nick = NormNick}}};

                Other ->
                    ?DEBUG("Error: nick could not be reserved: ~p~n", [Other]),
                    irc_client_helpers:send(State, "433", Nick, [":Nick already in use, choose another one"]),
                    {next_state, registering_nick, irc_client_ping_pong:reset_timer(State)}
            end;

        invalid ->
            ?DEBUG("Error: invalid nick name ~p", [Nick]),
            irc_client_helpers:send(State, "432", Nick, [":Error in nick name, choose another one"]),
            {next_state, registering_nick, irc_client_ping_pong:reset_timer(State)}
    end.


-spec user(#client_state{}, string(), string(), string(), string()) -> {next_state, ready, #client_state{}}.
user(State, Username, Param1, _, Realname) ->
    case irc_utils:valid_user(Username) of
        valid ->
            Usr = State#client_state.user,

            case irckerl_utils:is_int_str(Param1) of
                true ->
                    {Mod, _} = string:to_integer(Param1),
                    UMode = int_mode_to_str(State#client_state.settings, Mod),

                    NState = State#client_state{
                               user = Usr#user{
                                        username = Username,
                                        realname = Realname,
                                        mode     = UMode
                                       }
                              };

                _ ->
                    NState = State#client_state{
                               user = Usr#user{
                                        username = Username,
                                        realname = Realname,
                                        mode     = proplists:get_value(std_umode, State#client_state.settings, "iwx")
                                       }
                              }
            end,

            Rslt = {
              next_state,
              ready,
              irc_client_ping_pong:reset_timer(NState)
             },

            send_first_messages(NState);

        invalid ->
            Rslt = {next_state, registering_user, State},
            irc_client_helpers:send(State, "461", [":Invalid username"])
    end,

    Rslt.



-spec join(#client_state{}, string() | [string()]) -> {next_state, ready, #client_state{}}.
join(State, "0") ->
    Chans = lists:map(fun(C) -> C#channel.name end, State#client_state.channels),
    part(State, Chans, "Leaving all channels"),
    {next_state, ready, irc_client_ping_pong:reset_timer(State#client_state{channels = []})};
join(State, Channels) ->
    Chans = State#client_state.channels ++ join_channels(State, Channels, []),
    {next_state, ready, irc_client_ping_pong:reset_timer(State#client_state{channels = Chans})}.

join(State, Channels, Passwords) ->
    Chans = State#client_state.channels ++ join_channels(State, Channels, Passwords),
    {next_state, ready, irc_client_ping_pong:reset_timer(State#client_state{channels = Chans})}.

-spec join_channels(#client_state{}, [string()], [string()]) -> [#channel{}].
join_channels(_, [], _) ->
    [];
join_channels(State, [Chan|Tail], Passwords) ->
    [Pass, PTail] = case length(Passwords) of
                        0 ->
                            [[], []];
                        _ ->
                            [L | MyTail] = Passwords,
                            [L, MyTail]
                    end,

    case gen_server:call(irckerl_controller, {get_channel, Chan, create}) of
        {ok, Channel} ->
            try_join(State, Chan, Channel, Pass, Tail, PTail);

        {ok, Channel, new} ->
            try_join(State, Chan, Channel, Pass, Tail, PTail);

        _ ->
            irc_client_helpers:send(State, "437", ["#", Chan, ":Nick/channel is temporarily unavailable"]),
            join_channels(State, Tail, PTail) % TODO: real error messages
    end.

try_join(State, Chan, Channel, Pass, Tail, PTail) ->
    case gen_server:call(Channel, {join, State#client_state.user, Pass}) of
        {ok, Names} ->
            Str = trim:trim(lists:map(fun(N) ->
                                              case irc_utils:has_mode($o, N#chan_user.mode) of
                                                  true ->
                                                      "@";
                                                  _ ->
                                                      ""
                                              end ++ N#chan_user.user#user.nick ++ " "
                                      end, Names)),

            irc_client_helpers:send(State#client_state.socket, [":", irc_utils:full_nick(State#client_state.user), " JOIN :", Chan, "\r\n"]),
            irc_client_helpers:send(State, "353", ["= ", Chan, " :", Str]),
            irc_client_helpers:send(State, "366", [Chan, " :End of NAMES list"]),
            [#channel{name = Chan, pid = Channel, normalized_name = irc_utils:to_lower(Chan)}] ++ join_channels(State, Tail, PTail);

        _ ->
            irc_client_helpers:send(State, "437", ["#", Chan, ":Nick/channel is temporarily unavailable"]),
            join_channels(State, Tail, PTail) % TODO: real error messages
    end.

-spec mode(#client_state{}, string()) -> {next_state, ready, #client_state{}}.
mode(State, Nick) ->
    case irc_utils:to_lower(Nick) == State#client_state.user#user.normalized_nick of
        true ->
            irc_client_helpers:send(State, "421", ["+", State#client_state.user#user.mode]),
            {next_state, ready, irc_client_ping_pong:reset_timer(State)};
        _ ->
            {next_state, ready, irc_client_ping_pong:reset_timer(State)}
    end.


-spec mode(#client_state{}, string(), string()) -> {next_state, ready, #client_state{}}.
mode(State, Nick, Mode) ->
    {add, Add, sub, Sub} = irc_utils:parse_mode_string(Mode),

    case irc_utils:to_lower(Nick) == State#client_state.user#user.normalized_nick of
        true ->
            AddedModes = lists:filter(
                           fun(X) when X =/= $o, X =/= $O ->
                                   string:chr(State#client_state.user#user.mode, X) == 0;
                              (_) ->
                                   false
                           end, Add),

            SubtractedModes = lists:filter(
                                fun(X) when X =/= $o, X =/= $O ->
                                        string:chr(State#client_state.user#user.mode, X) =/= 0;
                                   (_) ->
                                        false
                                end, Sub),

            NMode = lists:filter(
                      fun(X) -> string:chr(SubtractedModes, X) == 0 end,
                      State#client_state.user#user.mode ++ AddedModes
                     ),

            case SubtractedModes ++ AddedModes of
                [] ->
                    {next_state, ready, irc_client_ping_pong:reset_timer(State)};
                _ ->
                    irc_client_helpers:send(State#client_state.socket, [":", State#client_state.user#user.nick, " MODE ", State#client_state.user#user.nick, " :+", NMode, "\r\n"]),
                    {next_state, ready, irc_client_ping_pong:reset_timer(State#client_state{user = State#client_state.user#user{mode = NMode}})}
            end;

        _ ->
            {next_state, ready, irc_client_ping_pong:reset_timer(State)}
        end.


-spec names(#client_state{}, string()) -> {next_state, ready, #client_state{}}.
names(State, Chan) ->
    case gen_server:call(irckerl_controller, {get_channel, Chan}) of
        {ok, Info} ->
            case gen_server:call(Info, get_users) of
                {ok, Users} ->
                    Str = trim:trim(lists:map(fun(#chan_user{user = #user{nick = Nick}, mode = Mode}) ->
                                                      case irc_utils:has_mode($o, Mode) of
                                                          true ->
                                                              "@";
                                                          _ ->
                                                              ""
                                                      end ++ Nick ++ " "
                                              end,
                                                  Users)),
                    irc_client_helpers:send(State, "353", ["= ", Chan, " :", Str]); % TODO: use @ for secret and * for private channels

                {error, Error} ->
                    ?ERROR("Error in get_users query for channel ~p: ~s~n", [Chan, Error])
            end;

        {error, Error} ->
            ?ERROR("Error in get_users query for channel ~p: ~s~n", [Chan, Error])
    end,

    irc_client_helpers:send(State, "366", [Chan, " :End of NAMES list"]),
    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.

-spec privmsg(#client_state{}, string(), string()) -> {next_state, ready, #client_state{}}.
privmsg(State, To, Message) ->
    case irc_utils:valid_channel(To) of
        valid ->
            case gen_server:call(irckerl_controller, {get_channel, To}) of
                {ok, Info} ->
                    case gen_server:call(Info, {privmsg, State#client_state.user#user.nick, irc_utils:full_nick(State#client_state.user), To, Message}) of
                        ok ->
                            ok;
                        {error, _Error} ->
                            irc_client_helpers:send(State, "437", [To, ":Could not send message"]) % TODO: correct error code
                    end;

                {error, _Error} ->
                    irc_client_helpers:send(State, "437", [":Could not find the channel ", To]) % TODO: correct error code/message
            end;

        _ -> % TODO: get user and send message
            case gen_server:call(irckerl_controller, {get_user, To}) of
                {ok, Info} ->
                    gen_fsm:send_event(Info#user.pid, {privmsg, irc_utils:full_nick(State#client_state.user), To, Message});

                {error, _Error} -> % TODO: check error reason and send specific message
                    irc_client_helpers:send(State, "401", [To, " :No such nick/channel"])
            end
    end,

    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.

-spec who(#client_state{}, string()) -> {next_state, ready, #client_state{}}.
who(State, "#" ++ Pattern) ->
    who_channel(State, "#" ++ Pattern);

who(State, "&" ++ Pattern) ->
    who_channel(State, "&" ++ Pattern);

who(State, Pattern) ->
    {ok, RePattern} = re:compile(irc_utils:from_irc_pattern(Pattern), [caseless]),
    Members = lists:filter(fun(User) -> irc_client_helpers:match_user(RePattern, User) end, irc_client_helpers:get_users_in_channels(State#client_state.channels)),
    who_users(State, Members).

-spec who_channel(#client_state{}, string()) -> {next_state, ready, #client_state{}}.
who_channel(State, Chan) ->
    case gen_server:call(irckerl_controller, {get_channel, Chan}) of
        {ok, Channel} ->
            Members = irc_client_helpers:get_users_in_channels(State#client_state.channels),

            ToCheckMembers = case gen_server:call(Channel, get_users) of
                                 {ok, Users} ->
                                     Users;
                                 _ ->
                                     []
                             end,
            Filtered = lists:filter(fun(User) ->
                                            lists:member(User, Members)
                                    end, ToCheckMembers),
            who_users(State, Filtered);

        _ ->
            who_users(State, [])
    end.


-spec who_users(#client_state{}, [#chan_user{}]) -> {next_state, ready, #client_state{}}.
who_users(State, Members) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    lists:map(
      fun(U) ->
              User = U#chan_user.user,
              % TODO: include channel names
              irc_client_helpers:send(State, "352", ["* ", User#user.username, " ", User#user.masked, " ", Host, " ", User#user.nick, " H :0 ", User#user.realname])
      end,
      Members
     ),

    irc_client_helpers:send(State, "315", [":End of /WHO list"]),
    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.




-spec ping(#client_state{}, atom(), string()) -> {next_state, atom(), #client_state{}}.
ping(State, SName, PingId) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    irc_client_helpers:send(State, ["PONG ", Host, " :", PingId]),
    {next_state, SName, irc_client_ping_pong:reset_timer(State)}.

-spec pong(#client_state{}, atom(), string()) -> {next_state, atom(), #client_state{}}.
pong(State, SName, Receiver) ->
    case Receiver == proplists:get_value(hostname, State#client_state.settings, "localhost") of
        true ->
            T = (irc_client_ping_pong:reset_timer(State))#client_state{ping_sent=false};
        _ ->
            case Receiver == State#client_state.no_spoof of
                true ->
                    T = (irc_client_ping_pong:reset_timer(State))#client_state{ping_sent=false};
                _ ->
                    T = irc_client_ping_pong:reset_timer(State)
            end
    end,
    {next_state, SName, T}.



%%
%% Internal functions
%%

-spec send_first_messages(#client_state{}) -> any().
send_first_messages(State) ->
    {created, {{Year, Month, Day}, {Hour, Minute, Second}}} = irc_client_helpers:send_server(created),
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    Lim = proplists:get_value(limits, State#client_state.settings, []),
    Set = State#client_state.settings, % Set is much less to type

    {visible, Visible, invisible, Invisible} = irc_client_helpers:send_server(count_users),
    {servers, Servers} = irc_client_helpers:send_server(count_servers),

    irc_client_helpers:send(State, "001", [":Welcome to the ", proplists:get_value(ircnetwork, Set, "ROXNet"), " IRC Network"]),
    irc_client_helpers:send(State, "002", [":Your host is ", Host, ", running IRCKErl V", ?VERSION]),
    irc_client_helpers:send(State, "003", [
                      ":This server was created at ",
                      integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), " ",
                      integer_to_list(Hour), ":", integer_to_list(Minute), ":", integer_to_list(Second)
                     ]),
    irc_client_helpers:send(State, "004", [
                      Host,
                      " IRCKErl",
                      ?VERSION, " ",
                      lists:map(fun({Mode, _, _}) -> Mode end, ?UMODES), " ",
                      lists:map(fun({CMode, _}) -> CMode end, ?CMODES)
                     ]
        ), % TODO: send implemented modes
    MChan = integer_to_list(proplists:get_value(maxchannels, Lim, 10)),
    irc_client_helpers:send(State, "005", [
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
    irc_client_helpers:send(State, "005", ["NETWORK=", proplists:get_value(ircnetwork, Set, "ROXNet"), " CASEMAPPING=ascii :are supported by this server"]),
    irc_client_helpers:send(State, "251", [":There are ", integer_to_list(Visible + Invisible), " and ", integer_to_list(Invisible), " users on ", integer_to_list(Servers), " servers"]),
    % TODO: send 255 :I have x clients and x servers
    % TODO: send 265 :Current Local Users: x  Max: x
    % TODO: send 266 :Current Global Users: x  Max: x
    case proplists:get_value(motd, Set, none) of
        none ->
            irc_client_helpers:send(State, "422", [":MOTD file is missing"]);
        Filename ->
            case file:read_file(Filename) of
                {ok, Data} ->
                    irc_client_helpers:send(State, "375", [":- ", proplists:get_value(ircnetwork, Set, "ROXNet"), " message of the day -"]),
                    lists:map(fun(Line) -> irc_client_helpers:send(State, "372", [":- ", Line]) end, re:split(trim:trim(binary_to_list(Data)), "\r\n|\r|\n")),
                    irc_client_helpers:send(State, "376", [":End of /MOTD command."]);

                _ ->
                    irc_client_helpers:send(State, "422", [":MOTD file is missing"])
            end
    end,
    irc_client_helpers:send(State#client_state.socket, [":", State#client_state.user#user.nick, " MODE ", State#client_state.user#user.nick, " :+", State#client_state.user#user.mode, "\r\n"]).

-spec topic(#client_state{}, string()) -> {next_state, ready, #client_state{}}.
topic(State = #client_state{channels = Channels}, Chan) ->
    case lists:filter(fun(C) -> C#channel.name == Chan end, Channels) of
        [TheChan] ->
            case gen_server:call(TheChan#channel.pid, topic) of
                {ok, none} ->
                    irc_client_helpers:send(State, "331", [Chan, " :No topic is set."]);

                {ok, Topic} ->
                    irc_client_helpers:send(State, "332", [Chan, " :", Topic#topic.topic]),
                    irc_client_helpers:send(State, "333", [Chan, " ", Topic#topic.author#user.nick, " ", integer_to_list(irckerl_utils:to_unixtimestamp(Topic#topic.updated))]);

                {error, _} ->
                    irc_client_helpers:send(State, "331", [Chan, " :No topic is set."])
            end;
        _ ->
            irc_client_helpers:send(State, "442", [Chan, " :You're not on that channel"])
    end,

    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.

-spec topic(#client_state{}, string(), string()) -> {next_state, ready, #client_state{}}.
topic(State = #client_state{channels = Channels}, Chan, NewTopic) ->
    case lists:filter(fun(C) -> C#channel.name == Chan end, Channels) of
        [TheChan] ->
            case gen_server:call(TheChan#channel.pid, {topic, NewTopic, State#client_state.user}) of
                ok ->
                    ok;

                {error, _} ->
                    irc_client_helpers:send(State, "482", [Chan, " :You're not channel operator"])
            end;
        _ ->
            irc_client_helpers:send(State, "442", [Chan, " :You're not on that channel"])
    end,

    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.


-spec version(#client_state{}) -> {next_state, ready, #client_state{}}.
version(State) ->
    Host = proplists:get_value(hostname, State#client_state.settings, "localhost"),
    irc_client_helpers:send(State, "351", [?VERSION, " ", Host, " :IRCKErl/", ?VERSION, " https://github.com/ckruse/irckerl"]),
    {next_state, ready, irc_client_ping_pong:reset_timer(State)}.


-spec part(#client_state{}, [string()]) -> {next_state, ready, #client_state{}}.
part(State, []) ->
    irc_client_helpers:send(State, "461", ["PART :need more params!"]),
    {next_state, ready, irc_client_ping_pong:reset_timer(State)};
part(State, [[]]) ->
    part(State, []);
part(State, Channels) ->
    part(State, Channels, "Leaving channel").

-spec part(#client_state{}, [string()], string()) -> {next_state, ready, #client_state{}}.
part(State, Channels, Reason) ->
    Chans = leave_channels(State#client_state.channels, Channels, State#client_state.user, Reason),
    {next_state, ready, irc_client_ping_pong:reset_timer(State#client_state{channels = Chans})}.


kick(State = #client_state{channels = Channels, user = #user{normalized_nick = Who}}, [Channel|Tail], [User|UTail], Reason) ->
    case lists:filter(fun(C) -> C#channel.name == Channel end, Channels) of
        [TheChan] ->
            case gen_server:call(TheChan#channel.pid, {kick, Who, User, Reason}) of
                ok ->
                    ok;

                {error, _} ->
                    irc_client_helpers:send(State, "482", [Channel, " :You're not channel operator"])
            end;
        _ ->
            irc_client_helpers:send(State, "442", [Channel, " :You're not on that channel"])
    end,

    kick(State, Tail, UTail, Reason);

kick(_, [], [], _) ->
    ok.



-spec leave_channels([#channel{}], [string()], #user{}, string()) -> [#channel{}].
leave_channels(Existing, [], _, _) ->
    Existing;
leave_channels(Existing, [Chan | Tail], User, Reason) ->
    NExisting = lists:filter(fun(C) ->
        case C#channel.name == Chan of
            true ->
                gen_server:call(C#channel.pid, {part, User, Reason}),
                false;
            _ ->
                true
        end
    end, Existing),
    leave_channels(NExisting, Tail, User, Reason).

int_mode_to_str(Settings, Mode) ->
    SMode = mode_int2str(Mode),
    case length(SMode) of
        0 ->
            proplists:get_value(std_umode, Settings, "iwx");
        _ ->
            SMode
    end.

mode_int2str(Mode) when Mode band 4 == 4 ->
    "i" ++ mode_int2str(Mode band bnot 4);
mode_int2str(Mode) when Mode band 8 == 8 ->
    "w" ++ mode_int2str(Mode band bnot 8);
mode_int2str(_) ->
    [].

%% eof
