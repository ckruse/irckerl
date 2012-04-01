{hostname,"irc.local.net"}.
{port,6668}.
{pingfreq,90}.
{motd,"motd.txt"}.
{std_umodes,"iwx"}.
{limits,[
         {maxchannels,100},
         {nicklen,30},
         {chanlen,32},
         {topiclen,307},
         {kicklen,307},
         {awaylen,307},
         {maxtargets,20},
         {maxusers,2500}
        ]
}.

{debug,true}.
{debug_modules,[
                irckerl_client,
                irckerl_channel,
                irckerl_app,
                irckerl_controller,
                irc_parser,
                irckerl_sup
               ]
}.
