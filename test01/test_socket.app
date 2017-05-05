{application, test_socket,
	[{description, "test socket application"},
	{vsn, "1.1.1"},
	{modules, []},
	{registered, []},
	{mod, {test_socket_app, []}},
	{application, [kernel, stdlib]},
	{env, [{port, 10000},
		{log, "/usr/local/log/beta_app.log"}]}]}.