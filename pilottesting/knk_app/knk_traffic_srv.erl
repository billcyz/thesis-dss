%% @author billcyz
%% @doc @todo Add description to knk_traffic_srv.


-module(knk_traffic_srv).

-export([start_link/1, start_link/2, start_link/3,
		 stop/1]).

%% -----------------------------------------------------------------------------

%% knk_traffic callback definition

-callback handle_traffic(Request :: term(), From :: {pid(), Tag :: term()}, 
							State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} | {noreply, NewState :: term()} | 
							{error, Reason :: {connect_error, timeout | refused} |
												{data_error, term()} |
												{type_error, term()},
							State :: term()}.

-callback handle_distribute(Request :: term(), From :: {pid(), Tag :: term()},
							State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} | {noreply, NewState :: term()} |
							{error, Reason :: term(), State :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()}),
								State :: term()) ->
	term().

%% -------------------------------------------------------------------------------

%% Start knk_traffic_srv
start_link(HandlerModule) ->
	start_link(HandlerModule, start_link).

start_link(HandlerModule, Fun) ->
	start_link(HandlerModule, Fun, []).

start_link(HandlerModule, Fun, Args) ->
	knk_traffic_sup:start_link(HandlerModule, Fun, Args).

%% Stop process
stop(Pid) ->
	exit(Pid, normal).

%% Open udp port for providing broadcast socket interface for node traffic
start_linkport(HandlerModule, Port) ->
	start_linkport(HandlerModule, Port, []).

start_linkport(HandlerModule, Port, Opts) ->
	knk_traffic_sup

%% Send synchronous broadcast message
sbcast()


