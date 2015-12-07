:- module(schedule, [schedule/1, legal_schedule/7],
	    [assertions]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- use_module(library(aggregates)).


schedule(MinTime) :-
	findall(G, legal_schedule(_A, _B, _C, _D, _E, _F, G), Time),
	earliest_completion(Time, MinTime).

legal_schedule(A, B, C, D, E, F, G) :-
	generator(7, 10, [A, B, C, D, E, F, G]),
	schedule_constraints(A, B, C, D, E, F, G).

schedule_constraints(A, B, C, D, E, F, G) :-
	precedence_constraints(A, B, C, D, E, F, G),
	distance_constraints(B, C, D).

precedence_constraints(A, B, C, D, E, F, G) :-
	B >= A + 1,
	C >= A + 1,
	D >= A + 1,
	E >= B + 5,
	E >= C + 3,
	F >= D + 5,
	F >= E + 2,
	G >= F + 1.

distance_constraints(B,  C, _D) :- C >= B + 1.
distance_constraints(_B, C, D) :- D >= C + 1.

generator(0, _, []).
generator(M, N, [Q|L]) :-
	M > 0,
	choose(N, Q),
	M1 is M -1,
	generator(M1, N, L).

choose(N, N) :- N > 0.
choose(N, M) :- N > 0, N1 is N -1, choose(N1, M).

earliest_completion([],       10000).
earliest_completion([T|Time], MinTime) :-
	earliest_completion(Time, MTime),
	min(T, MTime, MinTime).

min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.