:- module(ex,
	[
	    nats/1,
	    nat/2,
	    odds/1,
	    even/1,
	    ones/1,
	    squares/1,
	    odd_squares/1,
	    even_squares/1,
	    cubes/1,
	    test_takeWhile/1,
	    test_dropWhile/1
	], 
	[fsyntax, hiord, lazy]).

:- use_module(library(lazy/lazy_lib), _).


:- lazy nats/1.
nats(X) :-
	nums_from(0, X).

nat(N, R) :-
	nats(L),
	take(N, L, R).	

:- lazy odds/1.
odds(X) :-
	nums_from_inc(1, 2, X).

:- lazy even/1.
even(X) :-
	nums_from_inc(0, 2, X).

:- lazy ones/1.
ones(X) :-
	repeat(1, X).

:- lazy squares/1.
squares(Z) :-
	nats(L),
	mapp(L, (_(X, Y) :- Y is X * X), Z).

:- lazy mapp/3.
mapp([], _, []).
mapp([X|Xs], P, [H|T]) :-
	P(X,H),
	mapp(Xs, P, T).

:- lazy odd_squares/1.
odd_squares(Z) :-
	odds(L),
	mapp(L, (_(X, Y) :- Y is X * X), Z).

:- lazy even_squares/1.
even_squares(Z) :-
	even(L),
	mapp(L, (_(X, Y) :- Y is X * X ), Z).

:- lazy cubes/1.
cubes(Z) :-
	nats(L),
	mapp(L, (_(X, Y) :- Y is X * X * X), Z).

test_takeWhile(Res) :-
	nums_from(0, L),
	takeWhile((_(X) :- X =< 5), L, Res).

test_dropWhile(Res) :-
	nums_from(0,L),
	dropWhile((_(X):-X=<7),L,L2),
	take(3,L2, Res).
