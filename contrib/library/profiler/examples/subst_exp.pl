:- module(subst_exp, [substitute/3], [assertions]).

:- doc(author, "Edison Mera").


substitute(A + B, Subs, NewA + NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A * B, Subs, NewA * NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A - B, Subs, NewA - NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A = B, Subs, NewA = NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute((A ** B), Subs, (NewA ** B)) :-
	integer(B),
	!,
	substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
	find_replacement(A, Subs, B),
	!.
substitute(A, _, A).

find_replacement(A, [A = B|_], B).
find_replacement(A, [_|Ys],    B) :- find_replacement(A, Ys, B).
