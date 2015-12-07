:- module(compact_list, [compact_list/2], [assertions, nativeprops]).

:- use_module(library(lists)).

compact_list_n(L, N, R) :-
	length(L1, N),
	append(L1, R1, L),
	compact_list_n_(L, L1, R1, N, R).

compact_list_n_(L, L1, R1, N, R) :-
	length(L2, N),
	append(L2, R2, R1),
	(
	    L1 == L2 ->
	    (compact_list_n_(R1, L2, R2, N, R) -> true ; R1 = R)
	;
	    L = [E|L0],
	    (compact_list_n(L0, N, R0) -> R = [E|R0] ; R = L)
	).

:- test compact_list(A, B) :
	(A = [1, 2, 2, 2, 2, 3, 3, 4, 3, 4, 3, 4, 3, 4, 1, 5, 7, 1, 5, 7])
	=> (B = [1, 2, 3, 4, 1, 5, 7]) + not_fails.

:- pred compact_list(L, R) : list(L) => list(R)
	# "Predicate that deletes repeated sequences in a list.".

compact_list(L, R) :-
	compact_list_(L, 1, R).

compact_list_(L, N, R) :-
	compact_list_n(L, N, R0) ->
	N1 is N + 1,
	compact_list_(R0, N1, R)
    ;
	L = R.
