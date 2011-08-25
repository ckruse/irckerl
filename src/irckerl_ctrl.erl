-module(irckerl_ctrl).
-author("Christian Kruse <cjk@wwwtech.de>").
-vsn("0.1").

-export([start/0, stop/0]).

start() ->
    application:start(irckerl,permanent).

stop() ->
    application:stop(irckerl).



% eof
