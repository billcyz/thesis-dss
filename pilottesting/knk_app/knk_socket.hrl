%% Default options for gen_tcp:listen/2
-define(GEN_TCP_SERVER_OPT, [binary,
							 {active, false},
							 {reuseaddr, true}]).

%% Default options for gen_udp:open/2 for broadcasting
-define(UDP_BROADCAST_OPT, [binary,
							{active, false},
							{reuseaddr, true},
							{broadcast, true}}]).

%% Max number of sockets at one time
-define(MAX_SOCKET_NUM, 5).

%% Max restart number
-define(MAX_RESTART_NUM, 1).

%% Max seconds
-define(MAX_RESTART_SECONDS, 5).

%% Default udp broadcast port
-define(BROADCAST_PORT, 4399).
