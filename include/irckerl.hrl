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

-type irc_params()  :: [[string()]].
-type irc_prefix()  :: {} | {string(), string(), string()} | {string()}.
-type proplist() :: [proplists:property()].

-define(DEFAULT_MAX_CLIENTS, 2048).
-define(TIMEOUT, 180000).
-define(VERSION,"0.1").

-include("logger.hrl").
-include("umodes.hrl").
-include("cmodes.hrl").

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

-record(chan_user, {
    level = 0 :: integer(),
    user  = #user{} :: #user{}
}).

-record(topic, {
    topic   = []   :: string(),
    updated = none :: calendar:datetime() | none,
    author  = none :: #user{} | none
}).
-record(channel, {
    name            = []       :: string(),
    normalized_name = []       :: string(),
    mode            = []       :: string(),
    limit           = 0        :: non_neg_integer(),
    topic           = none     :: #topic{} | none,
    members         = []       :: [#chan_user{}],
    pid             = none     :: pid() | none,
    invite_list     = []       :: [{erlang:timestamp(), #user{}}] | none,
    password        = []       :: string()
}).

-record(channel_state, {
    channel  = none :: #channel{} | none,
    settings = none :: proplist() | none
}).

-record(controller_state, {
    max_clients      = ?DEFAULT_MAX_CLIENTS,
    listen_socket    = none      :: inet:socket() | none,
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
-record(irc_cmd, {
    prefix = {} :: irc_prefix(),
    target = {} :: irc_prefix(),
    cmd    = "" :: string(),
    params = [] :: irc_params()
}).

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
