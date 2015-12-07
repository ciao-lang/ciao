:- module(_, [approx/1, cost_type/1, resource/1, cost_expression/1, rel_t/1],
	    [assertions]).

:- prop approx/1 + regtype.
approx(ub).
approx(lb).
approx(me).
approx(oub).
approx(olb).
approx(o).
approx(ome).

:- prop cost_type/1 + regtype.
cost_type(sol(I)) :- nnegint(I).
cost_type(allsols).
cost_type(call).
cost_type(call_exit).
cost_type(call_fail).
cost_type(redo).
cost_type(redo_exit).
cost_type(redo_fail).

:- prop resource/1 + regtype.

resource(T) :- term(T).

:- prop cost_expression/1 + regtype.

cost_expression(T) :- term(T).

:- prop rel_t/1 + regtype.

rel_t(abs).
rel_t(rel).
