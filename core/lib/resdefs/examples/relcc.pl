:- module(relcc, _, [assertions, nativeprops,
		predefprf(prf_costcenter),
		predefprf(prf_exectime),
		profiler]).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- doc(module, "Example of integration between runtime-checks and
	profiler.  This tests relative usage of resources.").

:- cost_center qsort1/2, qsort2/2.

:- check pred qsort/2 + rel_cost(ub, call_exit, cost_center(
		qsort1/2,
		qsort2/2,
		res(ticks, call_exit)),
	    sum($(1), 3, 6, sum($(2), 7, 8, $(1) * $(2) +2) +1) /10000).

:- check pred qsort/2 + rel_cost(ub, call_exit, cost_center(
% 		qsort1/2,
%  		append/3,
		qsort2/2,
		res(ticks, call_exit)), 3 + 2).

:- check pred qsort/2 + rel_cost(ub, call_exit, cost_center(
% 		qsort1/2,
% 		qsort2/2,
		res(ticks, call_exit)), (0.25 + 1.5) /1000).

qsort1(A, B) :- qsort(A, B).
qsort2(A, B) :- qsort(A, B).

qsort([],    []).
qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort1(L1, R1),
	qsort2(L2, R2),
	append(R1, [X|R2], R).

partition([],    _, [],     []).
partition([H|L], X, [H|L1], L2) :-
	H < X, !,
	partition(L, X, L1, L2).
partition([H|L], X, L1, [H|L2]) :-
	H >= X,
	partition(L, X, L1, L2).

append([],    B, B).
append([H|A], B, [H|C]) :-
	append(A, B, C).
