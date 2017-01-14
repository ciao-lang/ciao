:- module(naturals, _, [lazy]).

:- use_module(library(lists), [nth/3]).


:- lazy naturals_list/1.
naturals_list(X) :-
	generate_naturals_list(1, X).

:- lazy generate_naturals_list/2.
generate_naturals_list(X, [X|List]) :-
	X1 is X + 1,
	generate_naturals_list(X1, List).

main(Position, Result) :-
	naturals_list(List),
	nth(Position, List, Result).
