:- module(getting_user_table, [main/0], [persdb_mysql]).

% This example dumps the contents of the 'user' table from the 'mysql' 
% database.

:- use_module(user_and_password).

%% sql_persistent_location(Table, DataBase Spec).
sql_persistent_location(user, db(mysql, User, Password, HP)):-
        mysql_host_and_port(HP),
        mysql_user(User),
        mysql_password(Password).


% Declaration of the table as a Prolog predicate

:- sql_persistent(
	user(string, string, string, string, string, string, string, string, string, string, string, string, string, string, string, string, string),
	user('Host', 'User', 'Password', 'Select_priv', 'Insert_priv', 'Update_priv', 'Delete_priv', 'Create_priv', 'Drop_priv', 'Reload_priv', 'Shutdown_priv', 'Process_priv', 'File_priv', 'Grant_priv', 'References_priv', 'Index_priv', 'Alter_priv'),
	user).


main :-
        % We access the tuples as calls to a Prolog predicate
	user(Host, User, Password, Select_priv, Insert_priv, Update_priv, Delete_priv, Create_priv, Drop_priv, Reload_priv, Shutdown_priv, Process_priv, File_priv, Grant_priv, References_priv, Index_priv, Alter_priv),
	display(user(Host, User, Password, Select_priv, Insert_priv, Update_priv, Delete_priv, Create_priv, Drop_priv, Reload_priv, Shutdown_priv, Process_priv, File_priv, Grant_priv, References_priv, Index_priv, Alter_priv)),
	nl,
        fail.

main :-
	display('DONE'), nl.
