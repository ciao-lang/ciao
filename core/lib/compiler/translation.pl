:- module(translation, [
        expand_term/4,
        expand_clause/6,
        goal_trans/3,
        add_sentence_trans_and_init/3,
        add_sentence_trans/3,
        del_sentence_trans/1,
        add_term_trans/3,
        del_term_trans/1,
        add_clause_trans/3,
        del_clause_trans/1,
        add_goal_trans/3,
        del_goal_trans/1
        ], [assertions, nortchecks]).

:- meta_predicate add_sentence_trans_and_init(+, spec, +).
:- meta_predicate add_sentence_trans(+, spec, +).
:- meta_predicate add_term_trans(+, spec, +).
:- meta_predicate add_clause_trans(+, spec, +).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(library(lists), [append/3]).

expand_term(X0, M, Dict, X2) :-
        sentence_translation(X0, M, Dict, X1), % not recursive
        term_translation(X1, M, Dict, X2).     % recursive

% ---------------------------------------------------------------------------

% sentence_translations(M, Trs) # "@var{Tr} is the list of sentence
%   translations for @var{M}"
:- data sentence_translations/2.

% (like add_sentence_trans/3, but invokes translation of '0', used for
%  translation initialization)
add_sentence_trans_and_init(M, S, Prior) :-
	check_priority(Prior),
        meta_spec_trans(M, S, Tr),
        do_translation(Tr, 0, [], _), % Initialize transl. for this module
        record_sentence_trans(M, Tr, Prior).

add_sentence_trans(M, S, Prior) :-
	check_priority(Prior),
        meta_spec_trans(M, S, Tr),
        record_sentence_trans(M, Tr, Prior).

record_sentence_trans(M, Tr, Prior) :-
        ( retract_fact(sentence_translations(M,Ts0)) -> true ; Ts0 = [] ),
	pqueue_insert(Ts0, Prior, Tr, Ts),
        asserta_fact(sentence_translations(M,Ts)).

del_sentence_trans(M) :-
        retractall_fact(sentence_translations(M,_)).

sentence_translation(X, M, Dict, Y) :-
        nonvar(X),
        sentence_translations(M, KVs),
	pqueue_values(KVs, [T|Ts]),
	!,
        do_sent_trans(Ts, T, X, Dict, Y).
sentence_translation(X, _, _, X).

do_sent_trans([], T, X, Dict, Xt) :-
        do_translation(T, X, Dict, Xt).
do_sent_trans([T|Ts], T0, X, Dict, Y) :-
        do_translation(T0, X, Dict, Xt),
        do_sent_trans2(Xt, T, Ts, Dict, Y).

do_sent_trans2([], _, _, _, []) :- !.
do_sent_trans2([S1|S2], T, Ts, Dict, St) :- !,
        do_sent_trans(Ts, T, S1, Dict, S1t),
        append_clauses(S1t, S2t, St),
        do_sent_trans2(S2, T, Ts, Dict, S2t).
do_sent_trans2(S, T, Ts, Dict, St) :-
        do_sent_trans(Ts, T, S, Dict, St).

append_clauses([], L, L) :- !.
append_clauses([C|Cs], L, [C|R]) :- !, append(Cs, L, R).
append_clauses(C, L, [C|L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data term_translations/2.

add_term_trans(M, S, Prior) :-
	check_priority(Prior),
        meta_spec_trans(M, S, Tr),
        ( retract_fact(term_translations(M,Ts0)) -> true ; Ts0 = [] ),
	pqueue_insert(Ts0, Prior, Tr, Ts),
        asserta_fact(term_translations(M,Ts)).

del_term_trans(M) :-
        retractall_fact(term_translations(M,_)).

term_translation(X, M, Dict, Y) :-
        term_translations(M, KVs),
	pqueue_values(KVs, Ts),
	!,
        term_translation_clauses(X, Ts, Dict, Y).
term_translation(X, _, _, X).

term_translation_clauses([], _, _, []) :- !.
term_translation_clauses([C|Cs], Ts, Dict, [D|Ds]) :- !,
        term_translation_t(C, Ts, Dict, D),
        term_translation_clauses(Cs, Ts, Dict, Ds).
term_translation_clauses(C, Ts, Dict, D) :-
        term_translation_t(C, Ts, Dict, D).

term_translation_t(X, _, _, Y) :- var(X), !, Y = X.
term_translation_t(X, Ts, Dict, Y) :-
        do_translations(Ts, X, Dict, Xt),
        functor(Xt, F, A),
        functor(Y, F, A),
        term_trans_args(A, Xt, Ts, Dict, Y).

term_trans_args(0, _, _, _, _) :- !.
term_trans_args(N, X, Ts, Dict, Y) :-
        arg(N, X, Xn),
        arg(N, Y, Yn),
        N1 is N-1,
        term_translation_t(Xn, Ts, Dict, Yn),
        term_trans_args(N1, X, Ts, Dict, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_clause(H, B, M, Dict, H1, B1) :-
        clause_translations(M, KVs),
	pqueue_values(KVs, Ts),
	!,
        do_translations(Ts, clause(H,B), Dict, clause(H1,B1)).
expand_clause(H, B,_M,_Dict, H, B).

:- data clause_translations/2.

add_clause_trans(M, S, Prior) :-
	check_priority(Prior),
        meta_spec_trans(M, S, Tr),
        ( retract_fact(clause_translations(M,Ts0)) -> true ; Ts0 = [] ),
	pqueue_insert(Ts0, Prior, Tr, Ts),
        asserta_fact(clause_translations(M,Ts)).

del_clause_trans(M) :-
        retractall_fact(clause_translations(M,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data goal_trans/3.

:- include(library(compiler/add_goal_trans)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta_spec_trans(M, S, Tr) :-
        term_to_meta(T/A, S),
        atom(T),
        create_trans(A, T, M, Tr).

create_trans(2, T,_M, T).
create_trans(3, T, M, Tr) :-
        functor(Tr, T, 1),
        arg(1, Tr, M).
create_trans(4, T, M, Tr) :-
        functor(Tr, T, 2),
        arg(1, Tr, M).

do_translations([], X, _, X).
do_translations([T|Ts], X, Dict, Y) :-
        do_translation(T, X, Dict, Xt),
        do_translations(Ts, Xt, Dict, Y).

do_translation(T, X, Dict, Y) :-
        comp_goal(T, Dict, G),
        arg(1, G, X),
        arg(2, G, Y),
        '$meta_call'(G),
	!.
do_translation(_, X, _, X).

comp_goal(T,_Dict, G) :-
        atom(T), !,
        functor(G, T, 2).
comp_goal(T,_Dict, G) :-
        functor(T, F, 1), !,
        arg(1, T, M),
        functor(G, F, 3),
        arg(3, G, M).
comp_goal(T, Dict, G) :-
        functor(T, F, 2), !,
        arg(1, T, M),
        functor(G, F, 4),
        arg(3, G, M),
        arg(4, G, Dict).

% ===========================================================================

% Enable to display a log of used translations

% :- export(log_translations/3).
% log_translations(Base, M, What) :-
% 	( ( What = term -> term_translations(M, KVs)
% 	  ; What = sentence -> sentence_translations(M, KVs)
% 	  ; What = goal -> findall(K-V, goal_trans(M, V, K), KVs)
% 	  ; What = clause -> clause_translations(M, KVs)
% 	  ) -> true
% 	; KVs = []
% 	),
% 	display(user_error, trpackagesfor(Base, M, What, KVs)), nl(user_error),
% 	nl(user_error).
