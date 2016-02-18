:- module(cost_test, _, _).


:- use_module(library(write)).
:- use_module(library(time_analyzer)).

my_pred.


main :-
	cost(my_pred, T, runtime),
	write('time cost: '), write(T), nl.
