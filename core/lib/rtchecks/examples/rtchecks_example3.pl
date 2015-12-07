:- module(_, _, [assertions, regtypes, nativeprops, rtchecks, expander]).

:- pred nullasr(A, B).

nullasr(_, _).

square(X, X2) :-
	X2 is X * X,
	check(X2 > 0).

% this should generate a runtime-check error:
test1 :-
	square(0, X2),
	display(X2),
	nl.

:- regtype animal/1.

animal(A) :- atm(A).

:- regtype family/1.

family(B) :- atm(B).

:- entry fullasr(A, B) : (animal(A), var(B)).

:- pred fullasr(A, B) :: atm(A) : (animal(A), atm(A)) => family(B) + not_fails.
:- pred fullasr(A, B) :: atm(A) : animal(A) => family(B) + is_det.
:- pred fullasr(A, B) :: int(A) : animal(A) => family(B) + is_det.

fullasr(_, _).
