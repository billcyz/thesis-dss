
%% application resource file

{application, com_db, [
	{description, "DSS database component"},
	{vsn, "1"},
	{modules, [com_db, com_db_sup, com_db_srv]},
	{registered, [com_db]},
	{application, [kernel, stdlib]},
	{mod, {com_db, []}},
	{env, []}
]}.