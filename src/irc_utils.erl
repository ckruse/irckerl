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

-module(irc_utils).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-export([to_lower/1, full_nick/1, valid_nick/2, valid_channel/1, has_mode/2, valid_user/1, from_irc_pattern/1, may/3]).

-include("irckerl.hrl").

% @doc A special to_lower/1 for IRC messages to handle special
% caracters correctly.
-spec to_lower(string() | []) -> string().
to_lower("[" ++ Rest)  -> "{" ++ to_lower(Rest);
to_lower("]" ++ Rest)  -> "}" ++ to_lower(Rest);
to_lower("\\" ++ Rest) -> "|" ++ to_lower(Rest);
to_lower([Head|Tail])  -> string:to_lower([Head]) ++ to_lower(Tail);
to_lower([])           -> [].


% @doc Combines a full nick from the nick name and additional informations.
-spec full_nick(#user{}) -> string().
full_nick(User) ->
    User#user.nick ++ "!" ++ User#user.username ++ "@" ++ User#user.masked.


% @doc Validates a nick, returns either the atom valid or invalid.
-spec valid_nick(string(), proplist()) -> valid | invalid.
valid_nick(Nick,Settings) ->
    Lim = proplists:get_value(limits,Settings,[]),

    case length(Nick) > proplists:get_value(nicklen,Lim,30) of
        true ->
            invalid;

        _ ->
            case re:run(Nick,"^[a-zA-Z_][a-zA-Z0-9_\\[\\]\\\\^{}`-]+$",[{capture,none}]) of %"
                match  ->
                    valid;

                _ ->
                    invalid
            end
    end.


-spec valid_channel(string() | binary()) -> valid | invalid.
valid_channel(Str) when is_list(Str) ->
    valid_channel(list_to_binary(Str));

valid_channel(<<"#", Token/binary>>) ->
    case (byte_size(Token) < 200) and (byte_size(Token) > 0) of
        true ->
            valid_channel_name(Token);
        false ->
            invalid
    end;

valid_channel(<<"&", Token/binary>>) ->
    case (byte_size(Token) < 200) and (byte_size(Token) > 0) of
        true ->
            valid_channel_name(Token);
        false ->
            invalid
    end;

valid_channel(_) ->
    invalid.

-spec valid_channel_name(binary()) -> valid | invalid.
valid_channel_name(<<" ", _/binary>>) ->
    invalid;
valid_channel_name(<<7, _/binary>>) -> % control g, not allowed
    invalid;
valid_channel_name(<<",", _/binary>>) ->
    invalid;
valid_channel_name(<<_:1/binary, Rest/binary>>) ->
    valid_channel_name(Rest);
valid_channel_name(<<>>) ->
    valid.


-spec valid_user(binary() | string()) -> valid | invalid.
valid_user(User) when is_list(User) ->
    valid_user(list_to_binary(User));

valid_user(User) ->
    case byte_size(User) of
        0 ->
            invalid;
        _ ->
            is_valid_user(User)
    end.


is_valid_user(<<"\0", _/binary>>) ->
    invalid;
is_valid_user(<<" ", _/binary>>) ->
    invalid;
is_valid_user(<<"\r", _/binary>>) ->
    invalid;
is_valid_user(<<"\n", _/binary>>) ->
    invalid;
is_valid_user(<<"@", _/binary>>) ->
    invalid;
is_valid_user(<<_:1/binary, Rest/binary>>) ->
    is_valid_user(Rest);
is_valid_user(<<>>) ->
    valid.



-spec has_mode(char(), #user{} | #channel{} | string()) -> boolean().
has_mode(Mode, User) when is_record(User, user) ->
    has_mode(Mode, User#user.mode);
has_mode(Mode, Chan) when is_record(Chan, channel) ->
    has_mode(Mode, Chan#channel.mode);

has_mode(_, []) ->
    false;

has_mode(Mode, MString) when is_list(MString) ->
    [C | Tail] = MString,

    case Mode == C of
        true ->
            true;
        _ ->
            has_mode(Mode, Tail)
    end.


from_irc_pattern(Pat) ->
    re:replace(
      re:replace(Pat, "[.()\\[\\]]", "\\\\&", [global, {return, list}]),
      "\\*",
      ".*",
      [global, {return, list}]
     ).


may(topic, #channel{mode = CMode}, #chan_user{mode = Mode}) ->
    case has_mode($t, CMode) of
        true ->
            has_mode($o, Mode);
        _ ->
            true
    end;

may(privmsg, #channel{mode = CMode}, #chan_user{mode = Mode}) ->
    case has_mode($m, CMode) of
        true ->
            case has_mode($o, Mode) of
                true ->
                    true;

                false ->
                    case has_mode($v, Mode) of
                        true ->
                            true;
                        _ ->
                            false
                    end
            end;

        _ ->
            true
    end;

may(kick, #channel{mode = CMode}, #chan_user{mode = Mode}) ->
    case has_mode($Q, CMode) of
        true ->
            false;
        _ ->
            case has_mode($o, Mode) of
                true ->
                    true;
                _ ->
                    false
            end
    end.




% eof
