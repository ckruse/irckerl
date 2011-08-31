-module(irckerl_ctrl).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-export([start/0, stop/0]).

% @doc This module is used to controll the application which
% means that you can use start/0 to start it and stop/0 to
% stop the application.

% @doc Starts the application.
start() ->
    application:start(irckerl,permanent).

% @doc Stops the application.
stop() ->
    application:stop(irckerl).



% eof
