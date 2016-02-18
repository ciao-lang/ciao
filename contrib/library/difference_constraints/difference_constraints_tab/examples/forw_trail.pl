:- module(forw_trail,
	[
	    p/1
	],[]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).
:- use_module(library(difference_constraints/difference_constraints_rt_ll)).


:- table t/1.

p(X) :-
	abolish_all_tables,
  	difference_constraints_var(X),
	t(X).

t(X) :-
	X #>= 0,
	Z #= X + 1,
	Y #= X + 1,
	t(Z),
	t(Y).

t(X) :- X #= 1.
	
