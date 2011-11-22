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

-module(irckerl_logger).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-behaviour(gen_server).

-include("irckerl.hrl").

-define(SERVER, ?MODULE).

% logging API
-export([debug/4, debug/5, s_debug/4, s_debug/5]).
-export([info/4, info/5, s_info/4, s_info/5]).
-export([warning/4, warning/5, s_warning/4, s_warning/5]).
-export([error/4, error/5, s_error/4, s_error/5]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Settings) ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [Settings], []) of % {debug, [trace]}
        {ok, Server} ->
            {ok, Server};

        {error, {already_started, Server}} ->
            {ok, Server};

        {error, Reason} ->
            error_logger:error_msg("Error starting logger: ~p~n",[Reason]),
            {error, Reason}
    end.

-spec init(proplist()) -> {ok, #logger_state{}}.
init(Settings) ->
    process_flag(trap_exit, true),

    {ok, Fd} = file:open(proplists:get_value(log_file, Settings, "./irckerl.log"),[append]),
    {ok, #logger_state{ settings = Settings, fd = Fd, level = proplists:get_value(log_level, Settings, debug) }}.


-spec handle_call(term(), _, #logger_state{}) -> {atom(), term(), #logger_state{}}.
handle_call({log, Level, Pid, Module, Line, Message}, _, State) when State#logger_state.level == debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {reply, ok, State};

handle_call({log, Level, Pid, Module, Line, Message}, _, State) when State#logger_state.level == info, Level =/= debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {reply, ok, State};

handle_call({log, Level, Pid, Module, Line, Message}, _, State) when State#logger_state.level == warning, Level =/= debug, Level =/= info ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {reply, ok, State};

handle_call({log, Level, Pid, Module, Line, Message, Args}, _, State) when State#logger_state.level == debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {reply, ok, State};

handle_call({log, Level, Pid, Module, Line, Message, Args}, _, State) when State#logger_state.level == info, Level =/= debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {reply, ok, State};

handle_call({log, Level, Pid, Module, Line, Message, Args}, _, State) when State#logger_state.level == warning, Level =/= debug, Level =/= info ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {reply, ok, State};


handle_call(_, _, State) ->
    {reply, ok, State}.




-spec handle_cast(term(), #logger_state{}) -> {noreply, #logger_state{}}.
handle_cast({log, Level, Pid, Module, Line, Message}, State) when State#logger_state.level == debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {noreply, State};

handle_cast({log, Level, Pid, Module, Line, Message}, State) when State#logger_state.level == info, Level =/= debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {noreply, State};

handle_cast({log, Level, Pid, Module, Line, Message}, State) when State#logger_state.level == warning, Level =/= debug, Level =/= info ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message),
    {noreply, State};


handle_cast({log, Level, Pid, Module, Line, Message, Args}, State) when State#logger_state.level == debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {noreply, State};

handle_cast({log, Level, Pid, Module, Line, Message, Args}, State) when State#logger_state.level == info, Level =/= debug ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {noreply, State};

handle_cast({log, Level, Pid, Module, Line, Message, Args}, State) when State#logger_state.level == warning, Level =/= debug, Level =/= info ->
    write_log(State#logger_state.fd, Level, Pid, Module, Line, Message, Args),
    {noreply, State};



handle_cast(_, State) ->
    {noreply, State}.


-spec handle_info(term(), #logger_state{}) -> {noreply, #logger_state{}}.
handle_info(_, State) ->
    {noreply, State}.

-spec code_change(term(), #logger_state{}, term()) -> {ok, #logger_state{}}.
code_change(_, State, _) ->
    {ok, State}.


-spec terminate(_, #logger_state{}) -> ok.
terminate(_, #logger_state{fd = Fd}) when Fd =/= undefined ->
    ?DEBUG("down with listener~n"),
    file:close(Fd),
    ok;

terminate(_, _) ->
    ?DEBUG("down with listener~n"),
    ok.


-spec s_debug(pid(), atom(), non_neg_integer(), string()) -> any().
-spec s_debug(pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
s_debug(Pid, Module, Line, Message) ->
    s_log(debug, Pid, Module, Line, Message).
s_debug(Pid, Module, Line, Message, Args) ->
    s_log(debug, Pid, Module, Line, Message, Args).

-spec debug(pid(), atom(), non_neg_integer(), string()) -> ok.
-spec debug(pid(), atom(), non_neg_integer(), string(), [any()]) -> ok.
debug(Pid, Module, Line, Message) ->
    log(debug, Pid, Module, Line, Message).
debug(Pid, Module, Line, Message, Args) ->
    log(debug, Pid, Module, Line, Message, Args).

-spec s_info(pid(), atom(), non_neg_integer(), string()) -> any().
-spec s_info(pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
s_info(Pid, Module, Line, Message) ->
    s_log(info, Pid, Module, Line, Message).
s_info(Pid, Module, Line, Message, Args) ->
    s_log(info, Pid, Module, Line, Message, Args).

-spec info(pid(), atom(), non_neg_integer(), string()) -> ok.
-spec info(pid(), atom(), non_neg_integer(), string(), [any()]) -> ok.
info(Pid, Module, Line, Message) ->
    log(info, Pid, Module, Line, Message).
info(Pid, Module, Line, Message, Args) ->
    log(info, Pid, Module, Line, Message, Args).

-spec s_warning(pid(), atom(), non_neg_integer(), string()) -> any().
-spec s_warning(pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
s_warning(Pid, Module, Line, Message) ->
    s_log(warning, Pid, Module, Line, Message).
s_warning(Pid, Module, Line, Message, Args) ->
    s_log(warning, Pid, Module, Line, Message, Args).

-spec warning(pid(), atom(), non_neg_integer(), string()) -> ok.
-spec warning(pid(), atom(), non_neg_integer(), string(), [any()]) -> ok.
warning(Pid, Module, Line, Message) ->
    log(warning, Pid, Module, Line, Message).
warning(Pid, Module, Line, Message, Args) ->
    log(warning, Pid, Module, Line, Message, Args).

-spec s_error(pid(), atom(), non_neg_integer(), string()) -> any().
-spec s_error(pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
s_error(Pid, Module, Line, Message) ->
    s_log(error, Pid, Module, Line, Message).
s_error(Pid, Module, Line, Message, Args) ->
    s_log(error, Pid, Module, Line, Message, Args).

-spec error(pid(), atom(), non_neg_integer(), string()) -> ok.
-spec error(pid(), atom(), non_neg_integer(), string(), [any()]) -> ok.
error(Pid, Module, Line, Message) ->
    log(error, Pid, Module, Line, Message).
error(Pid, Module, Line, Message, Args) ->
    log(error, Pid, Module, Line, Message, Args).

-spec log(debug | info | warning | error, pid(), atom(), non_neg_integer(), string()) -> ok.
-spec log(debug | info | warning | error, pid(), atom(), non_neg_integer(), string(), [any()]) -> ok.
log(Level, Pid, Module, Line, Message) ->
    gen_server:cast(?MODULE, {log, Level, pid_to_list(Pid), Module, Line, Message}).
log(Level, Pid, Module, Line, Message, Args) ->
    gen_server:cast(?MODULE, {log, Level, pid_to_list(Pid), Module, Line, Message, Args}).

-spec s_log(debug | info | warning | error, pid(), atom(), non_neg_integer(), string()) -> any().
-spec s_log(debug | info | warning | error, pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
s_log(Level, Pid, Module, Line, Message) ->
    gen_server:call(?MODULE, {log, Level, pid_to_list(Pid), Module, Line, Message}).
s_log(Level, Pid, Module, Line, Message, Args) ->
    gen_server:call(?MODULE, {log, Level, pid_to_list(Pid), Module, Line, Message, Args}).

-spec write_log(file:io_device(), debug | info | warning | error, pid(), atom(), non_neg_integer(), string()) -> any().
-spec write_log(file:io_device(), debug | info | warning | error, pid(), atom(), non_neg_integer(), string(), [any()]) -> any().
write_log(Fd, Level, Pid, Module, Line, Message) ->
    {{Year, Mon, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    io:fwrite(
        Fd,
        "[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B] ~s [~s:~B] [~s] ~s~n",
        [Year, Mon, Day, Hour, Min, Sec, Pid, Module, Line, atom_to_list(Level), Message]
    ).

write_log(Fd, Level, Pid, Module, Line, Message, Args) ->
    {{Year, Mon, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    file:write(
        Fd, [
            io_lib:format(
                "[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B] ~s [~s:~B] [~s] ",
                [Year, Mon, Day, Hour, Min, Sec, Pid, Module, Line, atom_to_list(Level)]
            ),
            io_lib:format(Message, Args),
            "\n"
        ]
    ).

% eof