:- module(free_var,
	[
	    p/1
	],[]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).

:- table t/1.

p(X) :-
	abolish_all_tables,
	t(X).

t(X) :-
	X #< 3,
	t(Z),
	X #= Z + 1.

t(X) :-
	X #= 1.
