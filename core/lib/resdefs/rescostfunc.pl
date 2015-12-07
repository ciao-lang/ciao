:- module(rescostfunc, [expand_cf/3, compact_cf/3, compound_size/3,
		compact_size/3],
	    [assertions, nativeprops, dcg]).

:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(sort)).

expand_cf(N,  [], N) :- num(N), !.
expand_cf(CF, IF, CFN) :-
	normalize_cf(CF, E0, CFN, E0, []),
	sort(E0, E1),
	map(E1, m_to_goal, IF).

compact_size(length(A, V),    V, length(A)) :- !.
compact_size(term_size(A, V), V, size(A)) :- !.
compact_size(A=V,             V, int(A)) :- !.
compact_size(G,               V, E) :-
	G =.. [F|A1],
	append(A, [V], A1),
	E =.. [F|A].

:- test compact_cf(ExprV, IF, Expr)
	: (ExprV=log(X), IF=[length(A, X)])
	=> (Expr = log(length(X))) + (is_det, not_fails).

compact_cf(ExprV, IF, Expr) :-
	member(G, IF),
	compact_size(G, ExprV1, Expr),
	ExprV == ExprV1,
	!.
compact_cf(AV + BV, G, A + B) :-
	!,
	compact_cf(AV, G, A),
	compact_cf(BV, G, B).
compact_cf(AV - BV, G, A - B) :-
	!,
	compact_cf(AV, G, A),
	compact_cf(BV, G, B).
compact_cf(AV * BV, G, A * B) :-
	!,
	compact_cf(AV, G, A),
	compact_cf(BV, G, B).
compact_cf(AV / BV, G, A / B) :-
	!,
	compact_cf(AV, G, A),
	compact_cf(BV, G, B).
compact_cf(+(AV), G, +(A)) :-
	!,
	compact_cf(AV, G, A).
compact_cf(log(AV), G, log(A)) :-
	!,
	compact_cf(AV, G, A).
compact_cf(-(AV), G, -(A)) :-
	!,
	compact_cf(AV, G, A).
compact_cf(sum(X, IniV, EndV, ExprV), G,
	    sum(Counter, Ini, End, Expr)) :-
	!,
	compact_cf(Ini, G, IniV),
	compact_cf(End, G, EndV),
	Counter = '$'(_N, X),
	compact_cf(Expr, [Counter|G], ExprV).
compact_cf(exp(Base, Exponent), G, exp(BaseV, ExponentV)) :-
	!,
	compact_cf(Base,     G, BaseV),
	compact_cf(Exponent, G, ExponentV).
compact_cf(fact(XV), G, fact(X)) :-
	!,
	compact_cf(XV, G, X).
compact_cf(sin(XV), G, sin(X)) :-
	!,
	compact_cf(XV, G, X).
compact_cf(cos(XV), cos(X), G) :-
	!,
	compact_cf(XV, X, G).
compact_cf(0.Inf, _, inf) :- !.
compact_cf(Num,   _, Num) :- num(Num).

m_to_goal(m(E, V), G) :- compound_size(E, V, G).

compound_size(length(A), V, length(A, V)) :- !.
compound_size(size(A),   V, term_size(A, V)) :- !.
compound_size(int(A),    V, A = V) :- !.
compound_size(E,         V, G) :-
	E =.. [F|A],
	append(A, [V], A1),
	G =.. [F|A1].

normalize_cf(Expr, _, ExprV) -->
	{var(Expr)},
	!,
	{Expr = ExprV}.
normalize_cf(A + B, G, AV + BV) -->
	!,
	normalize_cf(A, G, AV),
	normalize_cf(B, G, BV).
normalize_cf(A - B, G, AV - BV) -->
	!,
	normalize_cf(A, G, AV),
	normalize_cf(B, G, BV).
normalize_cf(A * B, G, AV * BV) -->
	!,
	normalize_cf(A, G, AV),
	normalize_cf(B, G, BV).
normalize_cf(A / B, G, AV/BV) -->
	!,
	normalize_cf(A, G, AV),
	normalize_cf(B, G, BV).
normalize_cf(+(A), G, +(AV)) -->
	!,
	normalize_cf(A, G, AV).
normalize_cf(log(A), G, log(AV)) -->
	!,
	normalize_cf(A, G, AV).
normalize_cf(-(A), G, -(AV)) -->
	!,
	normalize_cf(A, G, AV).
normalize_cf(sum(Counter, Ini, End, Expr), G,
	    sum(X, IniV, EndV, ExprV)) -->
	!,
	normalize_cf(Ini,  G,                 IniV),
	normalize_cf(End,  G,                 EndV),
	normalize_cf(Expr, [m(Counter, X)|G], ExprV).
normalize_cf(exp(Base, Exponent), G, exp(BaseV, ExponentV)) -->
	!,
	normalize_cf(Base,     G, BaseV),
	normalize_cf(Exponent, G, ExponentV).
normalize_cf(fact(X), G, fact(XV)) -->
	!,
	normalize_cf(X, G, XV).
normalize_cf(sin(X), G, sin(XV)) -->
	!,
	normalize_cf(X, G, XV).
normalize_cf(cos(X), G, cos(XV)) -->
	!,
	normalize_cf(X, G, XV).
normalize_cf(inf,  _, 0.Inf) --> !, [].
normalize_cf(Num,  _, Num) --> {num(Num)}, !, [].
normalize_cf(Term, G, X) -->
	{
	    element(m(E, V), G),
	    E==Term ->
	    X = V
	}
    ;
	[m(Term, X)].

element(_, L) :-
	var(L),
	!,
	fail.
element(X, [Y|_]) :-
	X = Y.
element(X, [_|L]) :-
	element(X, L).
