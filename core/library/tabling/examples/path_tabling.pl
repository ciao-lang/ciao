:- module(path_tabling,_).

:- use_package(tabling).

:- table path/2.

path(X,Y) :-
	path(X,Z),
	edge(Z,Y).
path(X,Y) :-
	edge(X,Y).

edge(a,b).
edge(b,a).
