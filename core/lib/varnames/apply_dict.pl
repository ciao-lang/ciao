:- module(apply_dict, [apply_dict/3, apply_dict/4, select_applicable/3,
		select_applicable/4],
	    [assertions, basicmodes, dcg, hiord, fsyntax, unittestdecls]).

:- use_module(library(varnames/dict_types)).

sel_member(E, Applicable) -->
	{member(E0, Applicable)},
	{E0 == E},
	!,
	[E].
sel_member(_, _) --> [].

sel_members([],    _) --> [].
sel_members([E|L], R) -->
	sel_member(E, R),
	sel_members(L, R).

:- pred select_applicable(?term, +varnamesl, -varnamesl).

select_applicable(Term, Dict, Applicable) :-
	select_applicable(Term, Dict, Applicable0, []),
	sel_members(Dict, Applicable0, Applicable, []). % To remove duplicates

select_applicable(Term, Dict) -->
	( {var(Term)} -> select_applicable_var(Term, Dict)
	; {functor(Term, _, N)},
	    select_applicable_args(N, Term, Dict) ).

select_applicable_var(Var, Dict) -->
	{member(Name = Value, Dict)},
	{Var == Value} ->
	[Name = Value]
    ;
	[].

select_applicable_args(0, _,    _) --> [], !.
select_applicable_args(N, Term, Dict) -->
	{arg(N, Term, Arg)},
	select_applicable(Arg, Dict),
	!,
	{N1 is N - 1},
	select_applicable_args(N1, Term, Dict).

:- pred apply_dict(?term, +varnamesl, ?term).

apply_dict(Term, Dict, PrettyTerm) :-
	apply_dict(Term, Dict, no, PrettyTerm).

:- load_test_module(library(varnames/dict_types)).

:- test apply_dict(T, D, Idemp, _) : (T=f(A, B), Idemp=yes, D=['C'=A, 'D'=B]).

:- prop apply_dict(?term, +varnamesl, ?, ?term).

:- success apply_dict(?Term0, +Dict, Idemp, Term)
	: (varnamesl(Dict), Idemp = yes)
	=> (apply_dict(Term, Dict, Idemp, Term1), Term = Term1)

# "The idempotent property: multiple applications of apply_dict/4 over
	any term gives the same term.".

apply_dict(Var0, Dict, _, Var) :-
	var(Var0),
	!,
	(
	    member(Name = Value, Dict),
	    Var0 == Value ->
	    ( var(Name) -> Var = '$VAR'('_')
	    ; Var = '$VAR'(Name)
	    )
	;
	    Var = Var0
	).
apply_dict('$VAR'(Term0), Dict, Idemp, '$VAR'(Term)) :-
	!,
	( atom(Term0) -> escape_varname(Idemp, Term0, Term)
	; apply_dict(Term0, Dict, Idemp, Term)
	).
apply_dict(Term0, Dict, Idemp, Term) :-
	functor(Term0, F, A),
	functor(Term,  F, A),
	apply_dict_args(A, Term0, Dict, Idemp, Term).

escape_varname(yes, Term,  Term).
escape_varname(no,  Term0, '$VAR'(Term)) :-
	escape_atom(Term0, Term).

escape_atom(Atom0, Atom) :-
	( atom_concat('''', _, Atom0) -> Atom0 = Atom1
	; atom_concat('''', Atom0, Atom1) ),
	( atom_concat(_, '''', Atom1) -> Atom = Atom1
	; atom_concat(Atom1, '''', Atom) ).

apply_dict_args(0, _,     _,    _,     _) :- !.
apply_dict_args(N, Term0, Dict, Idemp, Term) :-
	arg(N, Term0, Arg0),
	arg(N, Term,  Arg),
	apply_dict(Arg0, Dict, Idemp, Arg),
	!,
	N1 is N - 1,
	apply_dict_args(N1, Term0, Dict, Idemp, Term).
