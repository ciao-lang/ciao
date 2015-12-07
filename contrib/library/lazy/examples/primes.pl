:- module(primes, [test_primes/2], [lazy]).

:- use_module(library(lazy/lazy_lib), [nums_from/2, take/3]).


:- lazy cut/2.
cut([], []).
cut([H | T], [H | T2]) :-
	cut_(H, T, S),
	!,
	cut(S, T2).

:- lazy cut_/3.
cut_(_, [], []) :- !.
cut_(H1, [H2 | T], [H2 | T2]) :-
	cut_(H1, T, T2),
	H2 mod H1 > 0.
cut_(H1, [H2 | T], L) :-
	cut_(H1, T, L),
	H2 mod H1 =:= 0.

:- lazy primes/1.
primes(X) :-
	nums_from(2, Z),
	cut(Z, X).

test_primes(N, R) :-
	primes(X),
	take(N, X, R).
