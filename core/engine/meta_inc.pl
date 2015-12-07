:- module(meta_inc, [meta_inc_args/3], [assertions]).

:- use_module(library(iso_misc), [compound/1]).

:- pred meta_inc_args/3 # "Given a meta specification, gets the arity
	of the predicate after the meta expansion.".

meta_inc_args(0, A, A) :- !.
meta_inc_args(Meta, A, A1) :-
        meta_inc_args_(A, Meta, A, A1).

meta_inc_args_(0, _, A, A) :- !.
meta_inc_args_(A, Meta, N, N_) :-
	compound(Meta),
	arg(A, Meta, Type),
	meta_inc(Type, N, N1),
        A1 is A-1,
        meta_inc_args_(A1, Meta, N1, N_).

meta_inc(addmodule(Type), N0, N) :-
	!,
	N1 is N0 + 1,
	meta_inc(Type, N1, N).
meta_inc(addterm(Type), N0, N) :-
	!,
	N1 is N0 + 1,
	meta_inc(Type, N1, N).
meta_inc(_, N, N).
