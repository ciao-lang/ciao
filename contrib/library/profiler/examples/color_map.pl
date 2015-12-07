:- module(_, [color_map_n/1, color_map/5], [assertions, regtypes]).

color_map(A, B, C, D, E) :-
	color(A), color(B), color(C), color(D), color(E),
	legal_coloring(A, B, C, D, E).

legal_coloring(A, B, C, D, E) :-
	A \== B,
	A \== C,
	A \== D,
	A \== E,
	c(B, C, D),
	C \== E.

c(X, Y, Z) :-
	X \== Y,
	X \== Z.

:- regtype color/1.

color(blue).
color(green).
color(orange).
color(red).
color(yellow).

:-use_module(library(between)).

color_map_n(X) :-
	between(1, X, _),
	color_map(_A, _B, _C, _D, _E),
	fail
    ;
	true.
