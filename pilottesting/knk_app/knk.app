%% should be put in the ebin/ folder

{application, knk, 
	[{description, "An Erlang app for communicating user defined apps and app components"},
	{id, "knk_1"},
	{mod, {knk_app, []}}
	{modules, [knk_app, knk_main_sup, ...]},
	{applications, [kernel,stdlib,sasl,xmerl,mnesia]},
	{start_phases, %% extra start procedure},
	{env,
		[{port, [7070]},
		{other, }]}
	{vsn, "0.1"},
	{registered, [knk_app]}
	]}.