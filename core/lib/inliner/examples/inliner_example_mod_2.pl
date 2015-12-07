:- module(_, [map/3, test6/1], [hiord, expander]).

:- meta_predicate map(?, pred(2), ?).

map([],     _, []).
map([A|As], P, [B|Bs]) :- P(A, B), map(As, P, Bs).

test6(A) :-
	test7(A).

test7(a).
test7(b).
test7(c).
