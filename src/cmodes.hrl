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

-define(MODE_LIMIT,1).
-define(MODE_VOICE,2).
-define(MODE_HALFOP,4).
-define(MODE_CHANOP,8).
-define(MODE_PRIVATE,16).
-define(MODE_SECRET,32).
-define(MODE_MODERATED,64).
-define(MODE_NOPRIVMSGS,128).
-define(MODE_TOPICLIMIT,256).
-define(MODE_INVITEONLY,512).
-define(MODE_KEY,1024).
-define(MODE_RGSTR,2048).
-define(MODE_RGSTRONLY,4096).
-define(MODE_NOCOLOR,8192).
-define(MODE_CHANPROT,16384).
-define(MODE_CHANOWNER,32768).
-define(MODE_OPERONLY,65536).
-define(MODE_ADMONLY,131072).
-define(MODE_LINK,262144).
-define(MODE_NOKICKS,524288).
-define(MODE_BAN,1048576).
-define(MODE_STRIP,2097152).
-define(MODE_EXCEPT,4194304).
-define(MODE_INVEX,8388608).
-define(MODE_NOKNOCK,16777216).
-define(MODE_NOINVITE,33554432).
-define(MODE_FLOODLIMIT,67108864).
-define(MODE_MODREG,134217728).
-define(MODE_NOCTCP,268435456).
-define(MODE_AUDITORIUM,536870912).
-define(MODE_ONLYSECURE,1073741824).
-define(MODE_NONICKCHANGE,2147483648).

-define(CMODES,[
                {'l',MODE_LIMIT},
                {'v',MODE_VOICE}
                {'h',MODE_HALFOP},
                {'o',MODE_CHANOP}
                {'p',MODE_PRIVATE},
                {'s',MODE_SECRET},
                {'m',MODE_MODERATED},
                {'n',MODE_NOPRIVMSGS},
                {'t',MODE_TOPICLIMIT},
                {'i',MODE_INVITEONLY},
                {'k',MODE_KEY},
                {'r',MODE_RGSTR},
                {'R',MODE_RGSTRONLY},
                {'c',MODE_NOCOLOR},
                {'a',MODE_CHANPROT},
                {'q',MODE_CHANOWNER},
                {'O',MODE_OPERONLY},
                {'A',MODE_ADMONLY},
                {'L',MODE_LINK},
                {'Q',MODE_NOKICKS},
                {'b',MODE_BAN},
                {'S',MODE_STRIP},
                {'e',MODE_EXCEPT},
                {'I',MODE_INVEX},
                {'K',MODE_NOKNOCK},
                {'V',MODE_NOINVITE},
                {'f',MODE_FLOODLIMIT},
                {'M',MODE_MODREG},
                {'C',MODE_NOCTCP},
                {'u',MODE_AUDITORIUM},
                {'z',MODE_ONLYSECURE},
                {'N',MODE_NONICKCHANGE}
               ]
       ).

% eof
