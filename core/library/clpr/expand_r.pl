:- module(expand_r, [expand/2], [assertions]).
% Expansion for clp(R).

:- doc(bug, "Duplicated functionality w.r.t. eval_r.pl, but can be
	avoided using inliner package --EMM").

:- use_module(library(clpr/eval_r)).

expand(arith_zero(Exp), G) :-
	% arith_eps(Eps),
	ieee754_eps(Eps),
	NEps is -Eps,
	( var(Exp) ->
	    G = (Exp =< Eps, Exp >= NEps)
	;
	    G = (Res is Exp, Res =< Eps, Res >= NEps)
	).

expand(arith_eval(A=:=B), G) :-
	arith_eps(Eps),
	G = (abs(B-A) =< Eps * (abs(A) + abs(B))).
expand(arith_eval(A<B), G) :-
	arith_eps(Eps),
	NEps is -Eps,
	G = (A-B < NEps * (abs(A) + abs(B))).
expand(arith_eval(A=<B), G) :-
	arith_eps(Eps),
	G = (A-B =< Eps * (abs(A) + abs(B))).
expand(arith_eval(B>A), G) :-
	arith_eps(Eps), NEps is -Eps,
	G = (A-B < NEps * (abs(A) + abs(B))).
expand(arith_eval(B>=A), G) :-
	arith_eps(Eps),
	G = (A-B =< Eps * (abs(A) + abs(B))).
expand(arith_eval(Exp, V), G) :-
	expand_arith_eval_2(Exp, V, G).

expand_arith_eval_2(Exp,       V, G) :- var(Exp), !, G = (V is Exp).
expand_arith_eval_2(min(A, B), V, G) :- !,
	G = (arith_eval(A, C), arith_eval(B, D), min(C, D, V)).
expand_arith_eval_2(max(A, B), V, G) :- !,
	G = (arith_eval(A, C), arith_eval(B, D), max(C, D, V)).
expand_arith_eval_2(asin(A), V, G) :- !, G = (arith_eval(A, B), asin(B, V)).
expand_arith_eval_2(acos(A), V, G) :- !, G = (arith_eval(A, B), acos(B, V)).
expand_arith_eval_2(tan(A),  V, G) :- !, G = (arith_eval(A, B), tan(B, V)).
expand_arith_eval_2(Exp,     V, G) :- G = (V is Exp).
