:- module(doccfg_tr, [doccfg_sent/3], [assertions]).

% TODO: translate into "modules implementing the doccfg interface"
% TODO: move to interfaces or its own module
:- doc(title, "Translation for doccfg").
:- doc(author, "Jose F. Morales").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3]).

doccfg_sent(0, [], Mod) :- !,
	clean_db(Mod).
doccfg_sent(end_of_file, Cs2, Mod) :- !,
	emit_sents(Mod, Cs),
	clean_db(Mod),
	% TODO: This should be automatic
	append(Cs, [end_of_file], Cs2). % (allow other translations)
doccfg_sent((:- default_def(F/A, Cs)), [], Mod) :- !,
	assertz_fact(default_def(F, A, Mod, Cs)).
doccfg_sent(C, _, Mod) :- !,
	norm_clause(C, H, _),
	functor(H, F, A),
	add_defined(Mod, F, A),
	fail. % (just assert, do not transform)

norm_clause((H0 :- B0), H, B) :- !, H = H0, B = B0.
norm_clause(H, H, true).

% default_def(F,A,M,Cs): Cs is the default definition for F/A in M
:- data default_def/4.
% defined(F,A,M): F/A defined in M
:- data defined/3.

clean_db(Mod) :-
	retractall_fact(default_def(_,_,Mod,_)),
	retractall_fact(defined(_,_,Mod)).

add_defined(Mod, F, A) :-
	( current_fact(defined(F, A, Mod)) ->
	    true
	; assertz_fact(defined(F, A, Mod))
	).

emit_sents(Mod, Cs) :-
	findall(C, emit_sent(C, Mod), Cs).

emit_sent(C, Mod) :-
	% Get default clauses for undefined predicates
	default_def(F, A, Mod, Cs),
	\+ defined(F, A, Mod),
	member(C, Cs).
emit_sent(end_of_file, _Mod).
