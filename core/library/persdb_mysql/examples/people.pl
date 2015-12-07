:- module(_, _, [persdb_mysql, fsyntax]).

% Some contributions from Guy-Noel Mathieu

:- use_module(library(write)).
:- use_module(library(format)).

:- use_module(user_and_password).

sql_persistent_location(people, db(people, User, Password, HP)):-
        mysql_host_and_port(HP),
        mysql_user(User),
        mysql_password(Password).

:- sql_persistent(
        people(string, string, int),   %% Prolog predicate and types
        people(name, sex, age),        %% Table name and attributes
        people).                       %% Database local id

% Low level MySQL interface.
:- use_module(library(persdb_mysql/mysql_client)).


main :-
        nl,
        display('Creating database'), nl,nl,
        create_people_db,
        nl,
        display('Inserting people'), nl,nl,
        insert_people,
        nl,
        display('Showing people'), nl,nl,
        show_people,
	display('Removing John'), nl,nl,
	remove_people(john,_Y,_Z),
        display('Showing people, after removing John'), nl,nl,
        show_people,
	remove_people(_X,female,_Z),
        display('Showing people, after removing female'), nl,nl,
	show_people.

% Create a database and a table of people.  Still needs to be ironed out.

create_people_db :-
        mysql_user(User),
        mysql_password(Password),
        mysql_host_and_port(HP),
	mysql_connect(HP, '', User, Password, DbConnection),
        
	write(~mysql_query(DbConnection,
	        "drop database if exists people")), nl,
	write(~mysql_query(DbConnection, "create database people")), nl,
	write(~mysql_query(DbConnection, "use people")), nl,
	write(~mysql_query(DbConnection,
	        "create table people(name char(16) not null, 
sex text, age int, primary key(name))")), nl,
	mysql_disconnect(DbConnection).

% Inserts people into the 'people' table.

male(john, 15).
male(peter, 24).
male(ralph, 24).
male(bart, 50).
female(kirsten, 24).
female(mary, 17).
female(jona, 12).
female(maija, 34).


%% Tuples are inserted as in the local Prolog dynamic database
insert_people :-
	(
            male(N, A),
            display('Inserting '), 
            display(male(N, A)), 
            nl,
            dbassertz_fact(people(N, male, A)), 
            fail 
        ; 
            true
        ),
	(
            female(N, A), 
            display('Inserting '), 
            display(female(N, A)), 
            nl,
            dbassertz_fact(people(N, female, A)),
            fail 
        ; 
            true
        ).

 %% Removes people from the 'people' table.

 %% Still not working in MySQL due to differences in SQL: working on it.

remove_people(A, B, C) :-
        dbretractall_fact(people(A, B, C)).
 
remove_people_2(A, B, C) :-
        dbretract_fact(people(A, B, C)),
        display('Removed row '), display(people(A, B, C)), nl,
	fail.
remove_people_2(_, _, _) :-
 	display('No more rows'), nl.

show_people :-
	people(Name, Sex, Age),
	display(people(Name, Sex, Age)),
	nl,
	fail.
show_people :-
	display('No more rows'), nl.
