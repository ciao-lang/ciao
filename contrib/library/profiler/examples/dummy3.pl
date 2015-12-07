:- module(dummy3, [main/0], []).

cp :- true.
cp :- display(so), nl.

hola :- true.
mundo :- true.
que :- true.
contais :- true.
como :- true.
estas :- true.

aa :- hola, cp, mundo, member(X, [1, 2]), que, contais, display(X), como,
	estas, !, bb.

bb :- cc.
bb :- cc.

cc :- dd.

dd :- ee, X=1, X=2.

ee :- true.

main :-
	aa ;
	(display(choice_), nl).

% main :- display(hola_mundo),nl,!.
