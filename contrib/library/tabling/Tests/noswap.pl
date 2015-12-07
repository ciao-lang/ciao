:- module(noswap,
	[
	    a/0,
	    b/1,
	    c/1,
	    d/1,
	    e/1,
	    t/3,
	    r/3,
	    s/3
	],[]).

%% SIMPLE BENCHMARKS WHERE SWAPPING OPERATION IS NOT NEEDED.
 %% :- use_package(library(tabling_swapping)).
:- use_package(library(tabling)).

:- table a/0.
:- table b/1.
:- table c/1.
:- table d/1.
:- table e/1.

:- table t/3, t2/1.
:- table r/3, r2/2.
:- table s/3, s2/2.

a.

b(1).

c(1).
c(2).

d(1).
d(X) :-
	X = 2,
	d(_).

e(1).
e(X) :-
	X = 2,
	e(_).
e(X) :-
	e(Y),
	Y = 2,
	X is Y + 1.

t(X,Y,Z) :-
	Y = 3,
	t2(Z),
	t(_,_,_),
	X = Y.
t(1,1,1).
t2(2).
 %% t(1,1,1).
 %% t(3,3,2).

r(X,Y,Z) :-
	Y = 3,
	r2(Z,Y),
	display(sale(r2(Z,Y))),nl,
	r(A,B,C),
	display(sale(r_recursive_from_r(A,B,C,X,Y,Z))),nl,
	X = Y,
	display(new_answer(r(X,Y,Z))),nl.
r(1,1,1) :- display(new_answer(r(1,1,1))),nl.

r2(2,Y) :- display(new_answer(r2(2,Y))),nl.
r2(X,X) :-
	r(A,B,C),
	display(sale(r_recursive_from_r2(A,B,C,X))),nl,
	display(new_answer(r2(X,X))),nl.
 %% r1(1,1,1).
 %% r1(3,3,2).
 %% r1(3,3,3).

s(X,Y,Z) :-
	Y = 3,
	s2(Z,Y),
	s(_,_,_),
	X = Y.
s(1,1,1).

s2(X,X) :-
	s(_,_,_).
s2(2,_).
 %% s1(1,1,1).
 %% s1(3,3,2).
 %% s1(3,3,3).