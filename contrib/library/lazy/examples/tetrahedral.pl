:- module(tetrahedral, [test/1], [lazy]).

:- lazy write_row/2.
write_row(X, X) :-
	display(X), nl.
write_row(X, Y) :-
	display(Y),
	display(' '),
	Y1 is Y + 1,
	write_row(X, Y1).

:- lazy tetrahedral/1.
tetrahedral(0).
tetrahedral(X) :-
	write_row(X, 1),
	X1 is X - 1,
	tetrahedral(X1).

test(N) :- tetrahedral(N).
