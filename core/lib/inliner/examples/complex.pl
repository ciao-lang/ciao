:- module(_, [evalc/2, evalf/2], []).

:- use_package(inliner).
:- use_package(expander).

:- inline complex/1.
:- unfold complex(yes).

complex(c(A, B)) :-
	num(A),
	num(B).

:- inline frac/1.
:- unfold frac(yes).

frac(A/B) :-
	num(A),
	num(B).

:- inline sum/3.
:- unfold sum(yes, yes, yes).

sum(c(A1, A2), c(B1, B2), c(C1, C2)) :-
	C1 is A1 + B1,
	C2 is A2 + B2.
sum(A1/A2, B1/B2, C1/C2) :-
	C1 is A1 * B2 + B1 * A2,
	C2 is B1 * B2.

:- inline dif/3.
:- unfold dif(yes, yes, yes).

dif(c(A1, A2), c(B1, B2), c(C1, C2)) :-
	C1 is A1 - B1,
	C2 is A2 - B2.
dif(A1/A2, B1/B2, C1/C2) :-
	C1 is A1 * B2 - B1 * A2,
	C2 is B1 * B2.

:- inline eval/2.
:- unfold eval(yes, yes).

eval(A+B, C) :-
	sum(A, B, C).
eval(A-B, C) :-
	dif(A, B, C).

:- unfold evalc(yes, yes).

evalc(A, B) :-
	complex(B),
	eval(A, B).

evalf(A, B) :-
	frac(B),
	eval(A, B).
