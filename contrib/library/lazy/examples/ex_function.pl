:- module(ex_function,
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

:- fun_eval arith(true).

:- lazy fun_eval nats/0.
nats := ~nums_from(0).

:- fun_eval nat/1.
nat(N) := ~take(N, nats).	

:- lazy fun_eval odds/0.
odds := ~nums_from_inc(1, 2).

:- lazy fun_eval even/0.
even := ~nums_from_inc(0, 2).

:- lazy fun_eval ones/0.
ones := ~repeat(1).

:- lazy fun_eval squares/0.
squares := ~lazy_map(nats, (_(X, Y) :- Y is X * X)).

:- lazy fun_eval odd_squares/0.
odd_squares := ~lazy_map(odds, (_(X, Y) :- Y is X * X)).

:- lazy fun_eval even_squares/0.
even_squares := ~lazy_map(even, (_(X, Y) :- Y is X * X)).

:- lazy fun_eval cubes/0.
cubes := ~lazy_map(nats, (_(X, Y) :- Y is X * X * X)).

:- fun_eval test_takeWhile/0.
test_takeWhile := ~takeWhile((_(X) :- X =< 5), ~nums_from(0)).

:- fun_eval test_dropWhile/0.
test_dropWhile := ~take(3, ~dropWhile((_(X) :- X =< 7), ~nums_from(0))).

