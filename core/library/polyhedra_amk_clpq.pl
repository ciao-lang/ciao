
:- module(polyhedra_amk_clpq, [project/3, convex_hull/6, polyhedra_widen/5]).

:- use_package(clpq).

clpqr_meta(X):-
	clpq_meta(X).
clpqr_entailed(X):-
	clpq_entailed(X).

:- include(library(polyhedra_amk)).


