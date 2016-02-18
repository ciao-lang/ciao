:- module(ta2, main/0, []).

:- use_module(library(time_analyzer)).

:- push_prolog_flag(unused_pred_warnings, no).
ite(0).
ite(N) :- N1 is N - 1, ite(N1).
:- pop_prolog_flag(unused_pred_warnings).

test2 :-
	benchmark(ite,
	    [(1, 1), (2, 2), (100, 100), (1000, 1000)], average, 100, runtime,
	    Times),
	display(Times), nl,
	generate_plot('output', [(Times, [])]).



main :-
	test2.


