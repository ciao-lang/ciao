:- module(_, [nrev/2], [assertions, regtypes, nativeprops, resprof,
		predefres(res_steps), rtchecks]).

:- use_module(library(lists), [append/3]).
:- trust pred append(_1, L, _2)
	: (list(_1, gnd), list(L, gnd), var(_2))
	=> ( list(_1, gnd), list(L, gnd), list(_2, gnd),
	    size(ub, _1, length(_1)), size(ub, L, length(L)),
	    size(ub, _2, length(L) + length(_1)) )
	+ cost(ub, steps, length(_1) + 1).

% :- entry append/3: list(gnd) * list(gnd) * var.
% append([],     L, L).
% append([E|Es], L, [E|R]) :- append(Es, L, R).

:- check pred nrev(_1, C)
	: (list(_1, gnd), var(C))
	=> ( list(_1, gnd), list(C, gnd), size(ub, _1, length(_1)),
	    size(ub, C, length(_1)) )
	+ cost(ub, steps, 0.5*exp(length(_1), 2) +1.5*length(_1) +1-1).

:- entry nrev/2: list(gnd) * var.

nrev([],    []).
nrev([B|A], C) :-
	nrev(A, D),
	append(D, [B], C).
