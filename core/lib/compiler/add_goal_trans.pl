% Good priority for a translation
% TODO: allow symbolic priorities?
%check_priority(default_priority) :- !.
check_priority(Prior) :- number(Prior), !.

:- use_module(library(aggregates), [findall/3]).

:- meta_predicate add_goal_trans(+, spec, +).
add_goal_trans(M, S, Prior) :-
	check_priority(Prior),
	term_to_meta(T/A, S),
        atom(T),
        functor(Tr, T, A),
        ( A = 3 -> arg(3, Tr, M) ; true),
	findall(K-V, goal_trans(M, V, K), KVs0),
	retractall_fact(goal_trans(M,_,_)),
	pqueue_insert(KVs0, Prior, Tr, KVs),
	add_all_goal_trans(KVs, M).
 
del_goal_trans(M) :-
        retractall_fact(goal_trans(M,_,_)).

add_all_goal_trans([], _).
add_all_goal_trans([Prior-Tr|KVs], M) :-
	assertz_fact(goal_trans(M, Tr, Prior)),
	add_all_goal_trans(KVs, M).

% ---------------------------------------------------------------------------
% Priority queues (for storing transformations in order)

:- push_prolog_flag(unused_pred_warnings, no).

% TODO: move to sort? or define other adt?
:- use_module(library(sort), [keysort/2]).

% Insert in order
pqueue_insert(A, K, V, B) :-
	% TODO: allow symbolic priorities?
%	( K0 = default_priority -> K = 500 ; K = K0 ),
%	display(user_error, pi0(A, K, V)), nl(user_error),
	pqueue_insert0(A, K, V, B).
%	display(user_error, pi1(B)), nl(user_error).
	
pqueue_insert0([], K, V, KVs) :- !, KVs = [K-V].
pqueue_insert0(KVs0, K, V, KVs) :-
	KVs0 = [KV0|KVs1], KV0 = K0-_,
	( K < K0 ->
	    KVs = [K-V|KVs0]
	; KVs = [KV0|RestKVs],
          pqueue_insert0(KVs1, K, V, RestKVs)
	).

% Obtain all the values of the priority queue
pqueue_values([], []).
pqueue_values([_-V|Xs], [V|Vs]) :-
	pqueue_values(Xs, Vs).

:- pop_prolog_flag(unused_pred_warnings).
