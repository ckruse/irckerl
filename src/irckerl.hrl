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

-define(DEFAULT_MAX_CLIENTS, 2048).
-define(TIMEOUT, 180000).

-define(DEBUG(X), error_logger:info_msg(X)).
-define(DEBUG(X, Y), error_logger:info_msg(X, Y)).

-define(ERROR(X), error_logger:error_msg(X)).
-define(ERROR(X, Y), error_logger:error_msg(X, Y)).

-define(VERSION,"0.1").

% a user consists of:
-record(user,{nick, normalized_nick, name, realname, username, host, ip, masked, mode, pid}).

% an irc command consists of:
-record(irc_cmd, {from, to, cmd, args}).

-record(topic, {topic, updated, author}).
-record(channel, {name, normalized_name, mode, topic, members, pid}).

% eof
