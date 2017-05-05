%% Default options for gen_tcp:listen/2
-define(GEN_TCP_SERVER_OPT, [binary,
							 {active, false},
							 {reuseaddr, true}]).

%% Max number of sockets at one time
-define(MAX_SOCKET_NUM, 5).

%% Max restart number
-define(MAX_RESTART_NUM, 1).

%% Max seconds
-define(MAX_RESTART_SECONDS, 5).

