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

-define(UMODE_INVISIBLE, 1).
-define(UMODE_OPER, 2).
-define(UMODE_WALLOP, 4).
-define(UMODE_FAILOP, 8).
-define(UMODE_HELPOP, 16).
-define(UMODE_REGNICK, 32).
-define(UMODE_SADMIN, 64).
-define(UMODE_ADMIN, 128).
-define(UMODE_SERVNOTICE, 256).
-define(UMODE_LOCOP, 512).
-define(UMODE_RGSTRONLY, 1024).
-define(UMODE_NOCTCP, 2048).
-define(UMODE_SERVICES, 4096).
-define(UMODE_HIDE, 8192).
-define(UMODE_NETADMIN, 16384).
-define(UMODE_COADMIN, 32768).
-define(UMODE_WHOIS, 65536).
-define(UMODE_KIX, 131072).
-define(UMODE_BOT, 262144).
-define(UMODE_SECURE, 524288).
-define(UMODE_VICTIM, 1048576).
-define(UMODE_DEAF, 2097152).
-define(UMODE_HIDEOPER, 4194304).
-define(UMODE_SETHOST, 8388608).
-define(UMODE_HIDEWHOIS, 16777216).

-define(UMODES, [
                 {"i", global, ?UMODE_INVISIBLE},
                 {"o", global, ?UMODE_OPER},
                 {"w", global, ?UMODE_WALLOP},
                 {"g", global, ?UMODE_FAILOP},
                 {"h", global, ?UMODE_HELPOP},
                 {"r", global, ?UMODE_REGNICK},
                 {"a", global, ?UMODE_SADMIN},
                 {"A", global, ?UMODE_ADMIN},
                 {"s", local,  ?UMODE_SERVNOTICE},
                 {"O", local,  ?UMODE_LOCOP},
                 {"R", global, ?UMODE_RGSTRONLY},
                 {"T", global, ?UMODE_NOCTCP},
                 {"S", global, ?UMODE_SERVICES},
                 {"x", global, ?UMODE_HIDE},
                 {"N", global, ?UMODE_NETADMIN},
                 {"C", global, ?UMODE_COADMIN},
                 {"W", global, ?UMODE_WHOIS},
                 {"q", global, ?UMODE_KIX},
                 {"B", global, ?UMODE_BOT},
                 {"z", global, ?UMODE_SECURE},
                 {"v", global, ?UMODE_VICTIM},
                 {"d", global, ?UMODE_DEAF},
                 {"H", global, ?UMODE_HIDEOPER},
                 {"t", global, ?UMODE_SETHOST},
                 {"p", global, ?UMODE_HIDEWHOIS}
                ]
       ).


-define(U_LEVEL_USER, 0).
-define(U_LEVEL_VOICE, 10).
-define(U_LEVEL_HOP, 20).
-define(U_LEVEL_OP, 30).
-define(U_LEVEL_COFOUNDER, 40).
-define(U_LEVEL_FOUNDER, 50).


% eof
