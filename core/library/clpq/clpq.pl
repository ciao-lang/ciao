:- package(clpq).
:- include(library(clpqr/clpqr_ops)).
:- use_module(library(clpq/clpq_rt)).
:- use_module(library(clpq/solver_q),
	    [solve_sign/2, solve_abs/2, solve_mix/4, solve_mult/3, solve_pow/3,
		solve_trig/3, inf/2, sup/2, minimize/1, maximize/1]).

:- multifile dump_constraints/3.

:- use_module(library(clpq/clpq_meta)).

:- load_compilation_module(library(clpq/clpq_tr)).
% :- add_term_trans(clpq_tr:translate_hash/2).
:- add_goal_trans(clpq_tr:translate_clpqr/2, 750). % TODO: Right priority?
