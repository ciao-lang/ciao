:- module(getting_table_types, [main/0], []).

:- use_module(library(persdb_mysql/mysql_client)).
:- use_package(persdb_mysql).
:- use_module(library(format)).
:- use_module(library(lists)).

 %% :- multifile issue_debug_messages/1.
 %% :- data issue_debug_messages/1.
 %% issue_debug_messages('db_client').

:- use_module(user_and_password).

database_name(mysql).
table_name(user).

main :-
        mysql_user(User),
        mysql_password(Password),
        mysql_host_and_port(HP),
        database_name(Db),
        table_name(Table),
	mysql_connect(HP, Db, User, Password, DbConnection),
	mysql_table_types(DbConnection, Table, Types),
	mysql_disconnect(DbConnection),
	format("Results: ~w \n",Types).
