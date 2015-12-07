:- module(persvalue, [
		persistent_file/2,
		load_file/2,
		save_file/2,
		group_to_string/2,
		show_values/1,
		save_group/1,
		retract_group/1,
		set_value/3,
		current_value/3],
	    [assertions, fsyntax]).

:- use_module(library(file_utils)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(dynamic)).
:- use_module(library(strings)).

:- use_module(library(persvalue/persvalue_base)).

% TODO: Bad indexing (only first argument)
:- data persistent_file/2.
:- data current_value/3.

:- doc(author, "Edison Mera").

:- doc(module, "This module provides support for store persistent
	values in a file.  The file is compatible with GNU @tt{make} and
	can be used to store persistent configuration values.").

get_value(Group, Name, Value) :-
	persistent_file(Group, FileName),
	( file_exists(FileName) ->
	    file_to_string(FileName, Strings)
	;
	    Strings = ""
	),
	get_value_from_string(Strings, Name, Value).

load_file(Group, FileName) :-
	retract_group(Group),
	assertz_fact(persistent_file(Group, FileName)),
	(
	    get_value(Group, NameS, ValueS),
	    atom_codes(Name,  NameS),
	    atom_codes(Value, ValueS),
	    assertz_fact(current_value(Group, Name, Value)),
	    fail
	;
	    true
	).

retract_group(Group) :-
	retractall(persistent_file(Group, _)),
	retractall(current_value(Group, _, _)).

save_file(Group, FileName) :-
	string_to_file(~list_concat([
		    "# -*- mode: Makefile; -*-\n",
		    "# Warning: This file has been created automatically\n\n",
		    ~group_to_string(Group)]), FileName).

group_to_string(Group, String) :-
	findall(Line, ( current_value(Group, Name, Value),
		atom_codes(Name,  NameS),
		atom_codes(Value, ValueS),
		list_concat([NameS, "=", ValueS, "\n"], Line) ), L),
	list_concat(L, String).

save_group(Group) :-
	persistent_file(Group, FileName),
	save_file(Group, FileName).

show_values(Group) :-
	write_string(~group_to_string(Group)).

set_value(Group, Name, Value) :-
	retractall(current_value(Group, Name, _)),
	assertz_fact(current_value(Group, Name, Value)).

% TODO: This is a test entry that should be in other module, disabled
% :- export(main/0).
% main :-
% 	load_file(test, 'test.txt'),
% 	show_values(test),
% 	assertz_fact(current_value(test, 'Eureka', 'Aristoteles')),
% 	save_file(test, 'test.txt').
%
% %% Contents of test file 'test.txt':
% % edison=fernando
% % mera=menendez
% % Eureka=Aristoteles
% % Eureka=Aristoteles
