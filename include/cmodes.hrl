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

-define(CMODE_LIMIT,1).
-define(CMODE_VOICE,2).
-define(CMODE_HALFOP,4).
-define(CMODE_CHANOP,8).
-define(CMODE_PRIVATE,16).
-define(CMODE_SECRET,32).
-define(CMODE_MODERATED,64).
-define(CMODE_NOPRIVMSGS,128).
-define(CMODE_TOPICLIMIT,256).
-define(CMODE_INVITEONLY,512).
-define(CMODE_KEY,1024).
-define(CMODE_RGSTR,2048).
-define(CMODE_RGSTRONLY,4096).
-define(CMODE_NOCOLOR,8192).
-define(CMODE_CHANPROT,16384).
-define(CMODE_CHANOWNER,32768).
-define(CMODE_OPERONLY,65536).
-define(CMODE_ADMONLY,131072).
-define(CMODE_LINK,262144).
-define(CMODE_NOKICKS,524288).
-define(CMODE_BAN,1048576).
-define(CMODE_STRIP,2097152).
-define(CMODE_EXCEPT,4194304).
-define(CMODE_INVEX,8388608).
-define(CMODE_NOKNOCK,16777216).
-define(CMODE_NOINVITE,33554432).
-define(CMODE_FLOODLIMIT,67108864).
-define(CMODE_MODREG,134217728).
-define(CMODE_NOCTCP,268435456).
-define(CMODE_AUDITORIUM,536870912).
-define(CMODE_ONLYSECURE,1073741824).
-define(CMODE_NONICKCHANGE,2147483648).

-define(CMODES,[
                {"l",?CMODE_LIMIT},
                {"v",?CMODE_VOICE},
                {"h",?CMODE_HALFOP},
                {"o",?CMODE_CHANOP},
                {"p",?CMODE_PRIVATE},
                {"s",?CMODE_SECRET},
                {"m",?CMODE_MODERATED},
                {"n",?CMODE_NOPRIVMSGS},
                {"t",?CMODE_TOPICLIMIT},
                {"i",?CMODE_INVITEONLY},
                {"k",?CMODE_KEY},
                {"r",?CMODE_RGSTR},
                {"R",?CMODE_RGSTRONLY},
                {"c",?CMODE_NOCOLOR},
                {"a",?CMODE_CHANPROT},
                {"q",?CMODE_CHANOWNER},
                {"O",?CMODE_OPERONLY},
                {"A",?CMODE_ADMONLY},
                {"L",?CMODE_LINK},
                {"Q",?CMODE_NOKICKS},
                {"b",?CMODE_BAN},
                {"S",?CMODE_STRIP},
                {"e",?CMODE_EXCEPT},
                {"I",?CMODE_INVEX},
                {"K",?CMODE_NOKNOCK},
                {"V",?CMODE_NOINVITE},
                {"f",?CMODE_FLOODLIMIT},
                {"M",?CMODE_MODREG},
                {"C",?CMODE_NOCTCP},
                {"u",?CMODE_AUDITORIUM},
                {"z",?CMODE_ONLYSECURE},
                {"N",?CMODE_NONICKCHANGE}
               ]
       ).

% eof
