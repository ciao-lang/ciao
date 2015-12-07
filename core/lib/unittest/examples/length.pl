:- module(length, [length/2, concat/3],
	    [assertions, isomodes, metatypes, hiord, nativeprops]).

:- doc(author, "Alvaro Sevilla San Mateo").

length([],    0).
length([_|T], N) :- length(T, N0), N is N0 + 1.

concat([],     L,  L).
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).

:- test length(X, Y) : (X = [1, 2, 3], Y = 3) + not_fails.
:- test length(X, Y) : (X = [a, b, c, d, e], Y = 5) + not_fails.
:- test length(X, Y) : (X = [], Y = 0) + not_fails.
:- test length(X, Y) : (X = [a, b, c, d, e], Y = 5) + not_fails.
:- test length(X, Y) : (X = [a, b, c], Y = 5) + fails.
:- test length(X, Y) : (X = [a, b, c, d], Y = 5) + fails.
:- test concat(A, B, C) : (A = [1, 2], B = [3], C = [1, 2, 3]) + not_fails.
:- test concat(A, B, C) : (A = [1, 2], B = [3], var(C)) => (C=[1, 2, 3])
	+ not_fails.
