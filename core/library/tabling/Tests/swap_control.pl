:- module(swap_control,
	[
	    m/2,
	    n/3,
	    o/3,
	    p/3
	],[]).

 %% :- use_package(library(tabling_swapping)).
:- use_package(library(tabling)).

:- table t/1.
:- table s/1.

m(X,Y) :- 
	display(calling(t1(X))),nl,
	t(X), 
	display(exiting(t1(X))),nl,
	display(calling(t2(Y))),nl,
	t(Y),
	display(exiting(t2(Y))),nl.
n(X,Y,Z) :- 
	display(calling(t1(X))),nl,
	t(X), 
	display(exiting(t1(X))),nl,
	Z = a, 
	display(calling(t2(Y))),nl,
	t(Y),
	display(exiting(t2(Y))),nl.
o(X,Y,Z) :- 
	display(calling(t1(X))),nl,
	t(X), 
	display(exiting(t1(X))),nl,
	(Z = a; Z = b), 
	display(calling(t2(Y),Z)),nl,
	t(Y),
	display(exiting(t2(Y))),nl.

p(X,Y,Z) :- 
	display(calling(t1(X))),nl,
	t(X), 
	display(exiting(t1(X))),nl,
	s(Z), 
	display(calling(t2(Y),Z)),nl,
	t(Y),
	display(exiting(t2(Y))),nl.

t(X) :- X = 1.
t(X) :- X = 2.

s(X) :- X = a.
s(X) :- X = b.
