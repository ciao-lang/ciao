:- load_resource_module(res_nargs(res_nargs_res)).

:- head_cost(ub, nargs, f_nargs).
:- head_cost(lb, nargs, f_nargs).

:- literal_cost(ub, nargs, 0).
:- literal_cost(lb, nargs, 0).

:- trust_default + cost(ub, nargs, 0).
:- trust_default + cost(lb, nargs, 0).
