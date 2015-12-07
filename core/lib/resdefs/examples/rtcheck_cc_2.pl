:- module(rtcheck_cc_2, [qsort/2], [assertions, nativeprops,
		predefprf(prf_costcenter),
		predefprf(prf_exectime),
		profiler, rtchecks, inliner, expander]).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- doc(module, "Example of integration between runtime-checks and
	profiler.").

/*

To get the runtime errors, just run the following commands in the Ciao shell:

?- use_module(library(profiler/profiler_utils)).
?- set_prolog_flag(runtime_checks,yes).
?- use_module(library(resdefs/examples/rtcheck_cc_2)).
?- profile(qsort([1,2,3,4,5],X)).

useful commands:
profile_reset
profile_dump

*/

/*
%:- use_module(library(resdefs/rtr_ticks), []).

:- pred qsort1/2 + cost(ub, exectime, 0).
% :- pred qsort2/2 + cost(ub, exectime, 0.177).

% :- pred qsort1/2 + cost(ub, call_exit, ticks, 100000).
% :- pred qsort2/2 + cost(ub, call_exit, ticks, 4000).

:- doc(bug, "See if the following have sense:

:- check(call_exit) pred qsort/2 + cost(ub, cost_center(
		qsort1/2,
		qsort2/2,
		res(counts, redo_fail)), 0).


:- pred profile/1 + cost(ub, call_exit, cost_center(
		qsort1/2,
		res(exectime, allsols)), 0).
").

:- check pred qsort/2 + cost(ub, call_exit, cost_center(
		qsort1/2,
		qsort2/2,
		res(counts, call_exit)), 0).

% :- pred profile/1 + cost(ub, call_exit, cost_center(
% 		qsort1/2,
% 		qsort2/2,
% 		res(counts, redo_fail)), 0).

% :- pred qsort1/2 + cost_center(ub, call_exit, exectime, 4).

% :- pred qsort1/2 + cost(ub, exectime, 4).

% :- pred qsort/2 + cost_center(ub, call_exit, qsort1/2, qsort2/2, res(counts, call_exit), 0).

*/

:- check pred qsort/2 + cost(ub, call_exit, cost_center(
		qsort2/2,
		qsort1/2,
		res(ticks, call_exit)), 0).

:- check pred qsort/2 + cost(ub, call_exit, cost_center(
% 		qsort1/2,
		qsort2/2,
		res(ticks, call_exit)), 0).

:- check pred qsort/2 + cost(ub, call_exit, cost_center(
% 		qsort1/2,
% 		qsort2/2,
		res(ticks, call_exit)), 0).

:- cost_center qsort/2, qsort1/2, qsort2/2.

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
