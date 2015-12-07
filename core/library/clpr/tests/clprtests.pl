:- module(_, _, [assertions, nativeprops, unittestdecls, clpr]).

% TODO: Incomplete, add more reversibility tests --EMM
:- pred test1(X, Y) : succeeds(abs(X) .=<. 2*atan(1)) => succeeds(Y .=. X)
	+ not_fails.

:- test test1(X, Y) : (X .=. 1) # "Test sin/asin reversibility".

test1(X, Y) :-
	X .=. asin(sin(asin(sin(asin(sin(Y)))))).

% Tests for numbers bellow and above a fixed epsilon value:

:- test energy(M, E) : succeeds(M .=. 9.1093829140e-31)
	=> succeeds(E .=. 8.187105069054182e-14) # "Energy of one electron".

:- test energy(M, E) : succeeds(M .=. 1.0)
	=> succeeds(M .=. 1.112650056053618e-17) # "Mass of 1 Joule".

c_speed(C) :- C .=. 299792458.
energy(M, E) :- c_speed(C), E .=. M * C**2.

:- test test2 + fails # "Excluded middle".

test2 :-
	A .>=. B,
	A .<. B.
