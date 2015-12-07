:- module(datadist, _, []).

:- use_module(library(random)).


interval(A, B, N, Data) :-
	interval_(A, B, 0, N, Data).

interval_(_A, B, N, N, [B]   ).
interval_(A,  B, I, N, [D|Ds]) :-
	D is (A * (N - I) + B * I) / N,
	J is I + 1,
	interval_(A, B, J, N, Ds).

uniform(A, B, D) :-
	random(R),
	D is A + (B - A) * R.

gaussian(Mean, StdDev, D) :-
	random(R1),
	random(R2),
	U1 is 2 * R1 - 1,
	S2 is U1**2 + (2 * R2-1) **2,
	(S2 < 1 ->
	    D is sqrt(-2* log(S2) /S2) * U1 * StdDev + Mean
	;
	    gaussian(Mean, StdDev, D)
	).
