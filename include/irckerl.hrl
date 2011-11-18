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

-type irc_params()  :: list(string()).
-type irc_prefix()  :: {Nick::string(), User::string(), Host::string()} | {Host::string()}.
-type irc_command() :: {Prefix::irc_prefix() | {}, Command::string(), Params::irc_params()}.
-type proplist() :: [proplists:property()].

-define(DEFAULT_MAX_CLIENTS, 2048).
-define(TIMEOUT, 180000).

-import(error_logger).

-define(DEBUG(X), irckerl_logger:debug(self(), ?MODULE, ?LINE, X)).
-define(DEBUG(X, Y), irckerl_logger:debug(self(), ?MODULE, ?LINE, X, Y)).

-define(INFO(X), irckerl_logger:info(self(), ?MODULE, ?LINE, X)).
-define(INFO(X, Y), irckerl_logger:info(self(), ?MODULE, ?LINE, X, Y)).

-define(WARNING(X), irckerl_logger:warning(self(), ?MODULE, ?LINE, X)).
-define(WARNING(X, Y), irckerl_logger:warning(self(), ?MODULE, ?LINE, X, Y)).

-define(ERROR(X), irckerl_logger:error(self(), ?MODULE, ?LINE, X)).
-define(ERROR(X, Y), irckerl_logger:error(self(), ?MODULE, ?LINE, X, Y)).

-define(VERSION,"0.1").

% a user consists of:
-record(user,{
    nick            = []   :: string(),
    normalized_nick = []   :: string(),
    name            = []   :: string(),
    realname        = []   :: string(),
    username        = []   :: string(),
    host            = []   :: string() | inet:ip_address(),
    ip              = none :: inet:ip_address() | none,
    masked          = []   :: string(),
    mode            = []   :: string(),
    pid             = none :: pid() | none
}).

-record(topic, {
    topic   = []   :: string(),
    updated = none :: calendar:datetime() | none,
    author  = none :: #user{} | none
}).
-record(channel, {
    name            = []   :: string(),
    normalized_name = []   :: string(),
    mode            = []   :: string(),
    topic           = #topic{},
    members         = []   :: [#user{}],
    pid             = none :: pid() | none
}).

-record(channel_state, {
    channel  = none :: #channel{} | none,
    settings = none :: proplist() | none
}).

-record(controller_state, {
    max_clients      = ?DEFAULT_MAX_CLIENTS,
    listen_socket    = undefined :: inet:socket() | none,
    listen_port      = none      :: integer() | none,
    listen_interface = none      :: inet:ip_address() | string() | none,
    listener_process = none      :: pid() | none,
    clients          = []        :: [#user{}],
    settings         = none      :: proplist() | none,
    reserved_nicks   = none      :: dict() | none,
    created          = none      :: calendar:datetime() | none,
    servers          = []        :: any(),
    channels         = none      :: dict() | none
}).

% an irc command consists of:
-record(irc_cmd, {from, to, cmd, args}).

-record(client_state, {
    user          = #user{} :: #user{},
    socket        = none    :: inet:socket() | none,
    settings      = []      :: proplist() | [],
    no_spoof      = []      :: string() | [],
    the_timer     = none    :: timer:tref() | none,
    last_activity = {}      :: erlang:timestamp() | {},
    ping_sent     = false   :: boolean(),
    away          = []      :: string(),
    channels      = []      :: [#channel{}]
}).

-record(logger_state, {
    fd       = undefined :: file:io_device() | undefined,
    settings = []        :: proplist(),
    level    = debug     :: debug | info | warning
}).

% eof
