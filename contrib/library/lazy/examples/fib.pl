:- module(fib, _, [lazy, hiord]).

:- use_module(library(lists), [nth/3]).
:- use_module(library(lazy/lazy_lib), [tail/2, zipWith/4]).

:- lazy fibs/1.
fibs([0, 1 | R]) :- 
	fibs(L1),
	L2 = L1,
	tail(L2, T),
	zipWith((_(X,Y,Z) :- Z is X + Y), L1, T, R).

main(N, Y) :-
	fibs(L),
	nth(N, L, Y).
