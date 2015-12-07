:- module(solver_q, [
        % nl_eval
	solve_sign/2,
        solve_abs/2,
        solve_mult/3,
        solve_mult/5,
        solve_pow/3,
        solve_trig/3,
        solve_mix/4,
        solve_mix/6,
        normalize_abs/4,
        normalize_mult/5,
        normalize_div/5,
        normalize_pow/5,
        normalize_trig/5,
        normalize_mix/6,
        % solve
        wrap_term/2,
        var_with_def/5,
        eqn_var_new/2,
        solve_one/2,
        solve_two/3,
        solve_lin/2,
        solve_ineq_lt/2,
        solve_ineq_le/2,
        solve_eq_nbs/4,
        ground_meta/2,
        eqn_type_mask/2,
        join_eqn_types/2,
        join_goals/3,
        swap/3,
        swap/4,
        strip_dep/2,
        simplex_reconsider/2,
        remove_redundancy/1,
        % nf
        normalize/2, normalize/3, normalize/4, normalize/5,
        nf_coeff_of/3,
        mult_linear_factor/3,
        delete_factor_ordered/4,
        nf_substitute/7,
        nf_order/2,
        add_linear_ff/5,
        add_linear_1f/4,
        add_linear_11/3,
        add_f_log/5,
        factor/3,
        factor_k/4,
        factor_kk/6,
	% simplex
	inf/2,
	sup/2,
	minimize/1,
	maximize/1 ], 
	[library(clpq/clpq_src)]).

:- set_prolog_flag(multi_arity_warnings, off).

:- use_module(library(clpq/clpq_attr)).

:- include('../clpqr/nl_eval').
:- include('../clpqr/solve').
:- include('../clpqr/simplex').
:- include('../clpqr/nf').
