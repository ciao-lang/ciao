:- module(swap,
	[
	    m/5,
	    n/5,
	    r/3,
	    s/3
	],[]).

 %% :- use_package(library(tabling_swapping)).
:- use_package(library(tabling)).

:- table t1/2, t2/2.

m(X,Y,Z,A,B) :- t1(X,Y), Z = 20, t1(A,B).
%% EXPECTED OUTPUT %%
 %% m(a,b,20,a,b).
 %% m(a,b,20,1,2).
 %% m(1,2,20,a,b).
 %% m(1,2,20,1,2).

n(X,Y,Z,A,B) :- t1(X,Y), Z = 20, t2(A,B).
%% EXPECTED OUTPUT %%
 %% n(a,b,20,a,b).
 %% n(a,b,20,1,2).
 %% n(1,2,20,a,b).
 %% n(1,2,20,1,2).
 
t1(X,Y) :- X = 1, t2(_,_), Y = 2.
t1(a,b).

t2(X,Y) :- X = 1, t1(_,_), Y = 2.
t2(a,b).


:- table r/3, r2/1.

r(X,Y,Z) :-
	X = 1,
	r2(Y),
	r2(Z).

r2(2).
r2(X) :- r(X,_,_).

%% EXPECTED OUTPUT %%
 %% r(1,2,2).
 %% r(1,2,1).
 %% r(1,1,2).
 %% r(1,1,1).

:- table s/3, s2/1.

s(X,Y,Z) :-
	X = 1,
	s2(Y),
	s2(Z).
s(4,_,_).

s2(2).
s2(X) :- s(X,_,_).

%% EXPECTED OUTPUT %%
 %% s(1,2,2).
 %% s(1,2,1).
 %% s(1,1,2).
 %% s(1,1,1).
 %% s(4,_,_).
 %% s(1,2,4).
 %% s(1,4,2).
 %% s(1,4,1).
 %% s(1,4,4).

