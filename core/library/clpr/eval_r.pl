:- module(_, [arith_eps/1, ieee754_eps/1, arith_zero/1, arith_eval/2,
		arith_eval/1, as_float/2, arith_eq/2], []).

:- use_module(library(clpr/clpr_attr), [get_attribute/2]).
:- use_module(library(clpqr/arith_extra),
	    [asin/2, acos/2, tan/2, min/3, max/3]).

% low level arithmetic for clp(r,q,z)
%
% arith_zero(+Exp)
% arith_eval(+Exp <rel> +Exp)
% arith_eval(+Exp, -Res)

arith_eps(1.0e-10).

% EPS = 1e-10 is from Monash machine.c

%                -eps   0  +eps
%   ---------------[----|----]----------------
%            < 0                  > 0
%      <-----------]         [----------->
%           =< 0
%      <---------------------]
%                                 >= 0
%                  [--------------------->

ieee754_eps(1.0e-308). % IEEE 754 standard

arith_zero(Exp) :-
	arith_eps(Eps),
	% ieee754_eps(Eps),
	NEps is -Eps,
	arith_eval(Exp, Res),
	Res =< Eps,
	Res >= NEps.

arith_eval(A=:=B) :-
	arith_eq(A, B).
arith_eval(A<B) :-
	arith_eps(Eps), NEps is -Eps,
	A-B < NEps * (abs(A) + abs(B)).
arith_eval(A=<B) :-
	arith_eps(Eps),
	A-B =< Eps * (abs(A) + abs(B)).
arith_eval(B>A) :-
	arith_eps(Eps), NEps is -Eps,
	A-B < NEps * (abs(A) + abs(B)).
arith_eval(B>=A) :-
	arith_eps(Eps),
	A-B =< Eps * (abs(A) + abs(B)).

arith_eq(A, B) :-
	arith_eps(Eps),
	abs(B-A) =< Eps * (abs(A) + abs(B)).

arith_eval(min(A, B), V) :- !, arith_eval(A, C), arith_eval(B, D), min(C, D, V).
arith_eval(max(A, B), V) :- !, arith_eval(A, C), arith_eval(B, D), max(C, D, V).
arith_eval(asin(A),   V) :- !, arith_eval(A, B), asin(B, V).
arith_eval(acos(A),   V) :- !, arith_eval(A, B), acos(B, V).
arith_eval(tan(A),    V) :- !, arith_eval(A, B), tan(B, V).
arith_eval(Exp,       Res) :- Res is Exp.

as_float(Exp, Float) :-
	var(Exp) -> get_attribute(Exp, float(Float)) ; Float is Exp.
