:- module(flat, [flat/2], [assertions]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program flattens a term into a list of atom.").


flat(X, [X]) :-
	atomic(X), !.
flat(X, [F|List]) :-
	functor(X, F, N),
	flat_(N, X, List).

flat_(0, _, []).
flat_(N, X, List) :-
	N > 0,
	arg(N, X, Arg),
	flat(Arg, List1),
	N1 is N -1,
	flat_(N1, X, List2),
	append(List1, List2, List).

append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).
