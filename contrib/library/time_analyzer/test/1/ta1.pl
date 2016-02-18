:- module(ta1, _, _).

:- use_module(library(time_analyzer)).

lineal_pred(X) :- X = "big string".

test1 :-
	performance(lineal_pred(_), graph(1000, 2000, 100), Times),
	div_times(Times, DivTimes),
	generate_plot('performance_test', [(DivTimes, [])]).

main :-
	test1.
