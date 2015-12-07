:- module(_, _, [inliner, expander]).

insert_symbol_entry(ST, Pred, Entry) :-
	var(ST),
	!,
	Entry = st(Pred),
	ST = [Entry|_].

:- inline test1/2.
:- unfold test1(yes, yes).

test1(A, A) :-
	var(A),
	!.
test1([],    []).
test1([_|L], [elem|R]) :-
	test1(L, R).

test2(X, Y) :-
	test1([a, _AA, c|_D], X),
	test1([a, _BB, c], Y).
