:- module(test, [main/0, mydisplay/1, numbers/1, member/2, complex_term/1], []).

main :-
	display('hello from prolog'), nl.

mydisplay(X) :- display('from prolog '), display(X), nl.

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

numbers(1).
numbers(2).
numbers(3).
numbers(4).
numbers(5).
numbers(10).
numbers(100).
numbers(10000).
numbers(10000000).

complex_term(f([1,3,4,[4,3,2]], 234234, a,bbbb)).
