:- module(getting_mysql_database, [main/0, main0/0, main1/0], []).
:- use_module(library(persdb_mysql/mysql_client)).
:- use_package(persdb_mysql).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(user_and_password).

 %% :- multifile issue_debug_messages/1.
 %% :- data issue_debug_messages/1.
 %% issue_debug_messages('db_client').

:- use_module(user_and_password).

% Let us show the tables of this database
database_name(mysql).

main :- main1.

% Using low-level commands

main0 :-
        mysql_user(User),
        mysql_password(Password),
        mysql_host_and_port(HP),
        database_name(Table),
	mysql_connect(HP, Table, User, Password, DbConnection),
	mysql_get_tables(DbConnection, Tables),
	mysql_disconnect(DbConnection),
	format("Results: ~w \n",Tables).


% Using an already done, higher-level command
	
main1 :-
        mysql_user(User),
        mysql_password(Password),
        mysql_host_and_port(HP),
        database_name(Table),
        DataBase = db(Table, User, Password, HP),
	sql_get_tables(DataBase, Tables),
	format("Results: ~w \n",Tables).
