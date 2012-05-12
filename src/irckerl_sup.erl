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

-module(irckerl_sup).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-behaviour(supervisor).

-export([start_link/0, init/1]).

% @doc This module is the supervisor for the application, it monitors
% different processes and if one of them crashes restarts it.

% @doc Gets the settings from a file and starts the supervisor process.
start_link() ->
    case file:consult("settings.erl") of
        {ok, Settings} ->
            Settings;
        {error, _} ->
            Settings = []
    end,
    supervisor:start_link(?MODULE, [Settings]).

% supervisor callbacks

% @doc Starts debugger if needed and as its supervisor it
% starts the irckerl process.
init([Settings]) ->
    case proplists:get_value(debug, Settings, false) of
        true ->
            start_debugger(Settings);
        _ ->
            ok
    end,

    {ok, {
        {one_for_one, 1, 60}, % restart only once per minute
            [
                {irckerl_logger, {irckerl_logger, start_link, [Settings]}, permanent, brutal_kill, worker, [irckerl_logger]},
                {irckerl_controller, {irckerl_controller, start_link, [Settings]}, permanent, brutal_kill, worker, [irckerl_controller]}
            ]
        }
    }.

% @doc Starts the debugger GUI.
start_debugger(Settings) ->
    i:im(),
    i:iaa([break,exit]),
    Mods = proplists:get_value(debug_modules, Settings, []),
    lists:map(fun(M) -> i:ii(M) end, Mods).

% eof
