:- module(_, [qsort/2], [assertions, nativeprops]).

:- doc(author, "Edison Mera").

:- doc(module, "This example illustrates how the unified approach
	for static verification, run-time checking and unit testing
	works. In this case, a bug have been introduced: in the call
	to append/3 by qsort/2, variables R1 and R2 are swapped.  Then
	we prove statically part of the assertions, for the part that
	cannot be verified statically we generate run-time checks,
	and finally, the program also has a test attached that
	exercises the run-time checks.").

:- prop sorted_num_list/1.
:- export(sorted_num_list/1).

sorted_num_list([]).
sorted_num_list([X]) :- number(X).
sorted_num_list([X, Y|Z]) :-
	number(X), number(Y), X<Y, sorted_num_list([Y|Z]).

:- calls partition(A, B, C, D) : (ground(A), ground(B)).
:- comp partition(A, B, C, D) : (list(A, num)) + (not_fails, is_det).
:- success partition(A, B, C, D) => (ground(C), list(D, num)).

partition([],    _B, [],        []).
partition([E|R], C,  [E|Left1], Right) :-
	E < C, !, partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C, partition(R, C, Left, Right1).

:- calls append(A, B, C) : (list(A, num), list(B, num)).

append([],    X, X).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

:- entry qsort(A, B) : (list(A, num), ground(A)).

:- test qsort(A, B) : (A = [5, 7, 2, 4, 3]) => (B = [2, 3, 4, 5, 7]).

:- calls qsort(A, B) : list(A, num).
:- comp qsort(A, B) : (list(A, num), ground(A), var(B)) + is_det.
:- success qsort(A, B) => (ground(B), sorted_num_list(B)).

qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2), qsort(L1, R1),
	append(R2, [X|R1], R). % There is a bug here!!!
qsort([], []).
