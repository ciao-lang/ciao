% :- module(bench1,[test/3],[expander]).
:- module(bench1,[test/3]).

:- use_package(library(difference_constraints)).
:- use_module(library(difference_constraints/difference_constraints_rt_ll)).

test(X,Y,Z) :-
	nl,display(1), nl,
	difference_constraints_print,
	X - Y #= 3,
	nl,display(2), nl,
	difference_constraints_print,
	Y #= Z,
	nl,display(3), nl,
	difference_constraints_print,
	Z #= 1,
	nl,display(fin),nl,
	difference_constraints_print,nl.