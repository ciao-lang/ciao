:- module(_, [test1/0, test2/1, test3/2], [hiord, inliner, expander]).

% :- inline_module(library(hiordlib), [map/3]).
:- use_module(.(inliner_example_mod_2), [test6/1]).
:- inline_module(.(inliner_example_mod_2), [map/3]).
% :- inline_module(inliner_example_mod_2).

% :- include(inliner_example_mod_2).

:- export(test4/3).
:- meta_predicate test4(?,pred(2),?).
test4(X, Y, Z) :-
	map(X, Y, Z).

:- export(test5/1).
test5(A) :-
	test6(A).

p(A,A).

test1 :-
	map([a,b,c], p, Q),
	display(Q),
	nl.

test2(X) :-
	map(X, p, Q),
	display(Q),
	nl.

test3(X, Y) :-
	map(X, p, Y),
	display(Y),
	nl.
