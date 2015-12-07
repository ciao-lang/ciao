:- head_cost(ub, steps, 1).
:- head_cost(lb, steps, 1).

:- literal_cost(ub, steps, 0).
:- literal_cost(lb, steps, 0).

:- trust_default + cost(ub, steps, 0).
:- trust_default + cost(lb, steps, 0).
