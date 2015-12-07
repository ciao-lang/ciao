
:- module(polyhedra_amk_clpr, [project/3, convex_hull/6, polyhedra_widen/5]).

:- use_package(clpr).

clpqr_meta(X):-
	clpr_meta(X).
clpqr_entailed(X):-
	clpr_entailed(X).

:- include(library(polyhedra_amk)).
