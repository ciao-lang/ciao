:- module(resdefs_rt, [eval_cf/2], [assertions]).

:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(numlists)).

:- doc(bug, "We are not using is/2 instead of eval_cf/2 due to the
        usage of sum/4, fact/1, exp/2 and inf/0 arithmetic functions,
        that are not part of is/2. --EMM").

eval_cf(Expr, _) :-
	var(Expr),
	!,
	fail.
eval_cf(N, N) :-
	num(N),
	!.
eval_cf(A + B, Value) :-
	!,
	eval_cf(A, AV),
	eval_cf(B, BV),
	Value is AV + BV.
eval_cf(A - B, Value) :-
	!,
	eval_cf(A, AV),
	eval_cf(B, BV),
	Value is AV - BV.
eval_cf(A * B, Value) :-
	eval_cf(A, AV),
	eval_cf(B, BV),
	Value is AV * BV.
eval_cf(A / B, Value) :-
	eval_cf(A, AV),
	eval_cf(B, BV),
	Value is AV / BV.
eval_cf(+(A), Value) :-
	eval_cf(A, Value).
eval_cf(-(A), Value) :-
	eval_cf(A, AV),
	Value is - AV.
eval_cf(sum(X, Ini, End, Expr), Value) :-
	eval_cf(Ini, IniV),
	eval_cf(End, EndV),
	findall(V,
	    (
		between(IniV, EndV, X),
		eval_cf(Expr, V)
	    ),
	    Vs),
	sum_list(Vs, Value).
eval_cf(exp(Base, Exponent), Value) :-
	eval_cf(Base,     BaseV),
	eval_cf(Exponent, ExponentV),
	Value is BaseV ** ExponentV.
eval_cf(fact(X), Value) :-
	eval_cf(X, XV),
	fact(XV, 1, Value).
eval_cf(sin(X), Value) :-
	eval_cf(X, XV),
	Value is sin(XV).
eval_cf(cos(X), Value) :-
	eval_cf(X, XV),
	Value is cos(XV).
eval_cf(log(X), Value) :-
	eval_cf(X, XV),
	Value is log(XV).
eval_cf(inf, 0.Inf).

fact(0, F,  F) :- !.
fact(N, F0, F) :-
	N > 0,
	!,
	F1 is F0 * N,
	N1 is N - 1,
	fact(N1, F1, F).
